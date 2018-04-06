#lang racket

(require (for-syntax rackunit
                     syntax/parse)
         rackunit
         "../lib/schemas.rkt")

; Expand the macro and assert that it fails with the given message
(define-syntax check-def-failure
  (syntax-parser
    [(_ def msg)
     (check-exn (regexp (syntax->datum #'msg))
                (lambda () (local-expand #'def 'module-begin null)))
     #'(void)]))

; We have to define this at the top level; doesn't work in tests
(define-game some-game
   [buttons LP MP HP LK MK HK]
   [tick-rate 60])

(test-case
 "declaring buttons"
 (check-equal? (list LP
                     MP
                     HP
                     LK
                     MK
                     HK)
               '(b1 b2 b3 b4 b5 b6)))

(test-case
 "declaring tick rate"
 (check-equal? TICK-RATE 60))

(test-case
 "errors"
 (check-def-failure (define-game skullgirls
                      [buttons LP MP HP LK MK HK LP]
                      [tick-rate 60])
                    "Duplicate button declared")
 (check-def-failure (define-game skullgirls
                      [buttons LP MP HP LK MK HK a b c d e]
                      [tick-rate 60])
                    "Excess button"))
