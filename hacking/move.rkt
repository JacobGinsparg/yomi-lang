#lang racket

(provide (rename-out [move-read read]
                     [move-read-syntax read-syntax]))

(define (move-read in)
  (parameterize ([current-readtable (make-move-readtable)])
    (read in)))
 
(define (move-read-syntax src in)
  (parameterize ([current-readtable (make-move-readtable)])
    (read-syntax src in)))

(define (make-move-readtable)
  (make-readtable (current-readtable)
                  #\m 'dispatch-macro read-move))

(define read-move
  (case-lambda
    [(ch in)
     (check-move (read in) in (object-name in))]
    [(ch in src line col pos)
     (check-move (read-syntax src in) in src)]))

(define move-regex
  (regexp "^([a-z]\\.)?((?:[1-9](?:\\([0-9]+\\))?)+)?([A-z]+(?:\\+[A-z]+)*(?:\\([0-9]+\\))?)$"))

(define (check-move val in src)
  (regexp-match #px"^\\s*" in) ; skip whitespace
  (unless (regexp-match move-regex in)
    (error 'move "Bad move syntax"))
  val)