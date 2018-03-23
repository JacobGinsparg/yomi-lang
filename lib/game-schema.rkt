#lang racket

#|
(define-game skullgirls
  [buttons LP MP ...]
  [tick-rate 60])


(define LP 'b1)
(define MP 'b2)
(define HP 'b3)
(define LK 'b4)
(define MK 'b5)
(define HK 'b6)

(define TICK-RATE 60)
|#

(provide define-game
         buttons
         tick-rate)

(require (for-syntax syntax/parse
                     racket))

(define buttons   0)
(define tick-rate 1)

(define-for-syntax button-length 8 #|(length buttons-remaining)|#)

(define-syntax define-game
  (syntax-parser
    #:literals (buttons tick-rate)
    [(_ name:id
        [buttons btn:id ...]
        [tick-rate tix])
     #:with r (datum->syntax #'name 'racket)
     #:with req (datum->syntax #'name 'require)
     (define btn-symbols (map syntax->datum (syntax->list #'(btn ...))))
     (validate-buttons btn-symbols)
     (define tr-id (datum->syntax #'name 'TICK-RATE))
     #`(module name r
         (req racket)
         (provide #,tr-id btn ...)
         (define buttons-remaining '(b1 b2 b3 b4 b5 b6 b7 b8))
         (define (allocate-button)
           (if (empty? buttons-remaining)
               (error 'allocate-button "no more buttons")
               (let ([next-button (first buttons-remaining)])
                 (set! buttons-remaining (rest buttons-remaining))
                 next-button)))
         (define #,tr-id tix)
         (define btn (allocate-button)) ...)]))

(define-for-syntax (validate-buttons btn-list)
  (let ([seen (mutable-set)])
    (for-each (lambda (btn)
                (if (set-member? seen btn)
                    (error 'define-game "Duplicate button declared: ~s" btn)
                    (set-add! seen btn)))
              btn-list)
    (when (> (length btn-list) button-length)
      (error 'define-game "Can only define ~s buttons, ~s declared" button-length (length btn-list)))))