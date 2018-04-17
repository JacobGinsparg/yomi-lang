#lang racket

(require rackunit
         "../lib/schemas.rkt"
         "helpers.rkt")


; We have to define this at the top level; doesn't work in tests
(define-game some-game
   [buttons LP MP HP LK MK HK]
   [tick-rate 60])

(test-case
 "declaring buttons"
 (check-equal? (list button:LP
                     button:MP
                     button:HP
                     button:LK
                     button:MK
                     button:HK)
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
                    "Excess button")
 ; Wrap in #%module-begin so this runs at the same level as some-game on line 17
 (check-def-failure (#%module-begin
                     (define-game x
                       [buttons a]
                       [tick-rate 1]))
                    "Game schema already defined"))
