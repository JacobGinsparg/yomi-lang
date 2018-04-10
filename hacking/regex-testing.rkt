#lang racket

(require (for-syntax syntax/parse
                     syntax/to-string
                     racket/syntax
                     racket))

(define blah (regexp "^([a-z]\\.)?((?:[1-9](?:<[0-9]+>)?)+)?([A-z]+(?:\\+[A-z]+)*(?:<[0-9]+>)?)$"))

(define m0 "j.236K")
(define m1 "623HP+HK")
(define m2 "4<30>6HP")
(define m3 "j.HK")
(define m4 "4<30>6HP+HK<10>")

(define match regexp-match)
(define match* regexp-match*)

(define (match-all regex . matches)
  (andmap (lambda (x) (regexp-match-exact? regex x)) matches))

#;(define-syntax define-move
  (syntax-parser
    [(_ exprs ...)
     (define the-string (first (match blah (syntax->string #'(exprs ...)))))
     (define move-id (format-id #'(exprs ...) "~a" the-string #:source #'(exprs ...)))
     (define new-id (datum->syntax #'(exprs ...) move-id))
     (displayln new-id)
     #`(define #,new-id #,the-string)]))

#;(define-move 4(30)6HP+HK(10))