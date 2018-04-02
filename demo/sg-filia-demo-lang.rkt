; Demonstration of how to use Yomi at the language level.

#|
Functionally, this should mirror the library demo, but use the briefer syntax
provided by the language. It should be revised as more language features are
built out.
|#

#lang racket

(require "../lib/yomi-lib.rkt"
         "../lib/uinput.rkt"
         "../lib/schemas.rkt")


; Skullgirls game schema

(define-game skullgirls
  [buttons LP MP HP LK MK HK]
  [tick-rate 60])

(require (submod "." skullgirls))

; ------------------------------------------------------------------------------

; Filia character schema
; (this is incomplete; just enough for simple testing)
(move 5LP 5 3 7 14)
(move 5MP 13 3 9 19)
(move 5HP 12 3 11 21)
(move 5LK 9 3 7 19)
(move 5MK 10 3 9 17)
(move 5HK 19 9 14 26)
(move 2LP 6 3 7 7)
(move 623MP+HP 4 33 34 75) ; expect these numbers to be inaccurate
(define fenrir 623MP+HP)
                                              

; ------------------------------------------------------------------------------

; Some combos! Finally!

(define basic-launcher (list 5LP
                             (cancel 5LP TICK-RATE)
                             5LP
                             (cancel 5LP TICK-RATE)
                             5LK
                             (cancel 5LK TICK-RATE)
                             5MP
                             (cancel 5MP TICK-RATE)
                             5HP))

(define basic-otg-fenrir (list 5HK
                               (link 5HK TICK-RATE)
                               2LP
                               (cancel 2LP TICK-RATE)
                               5MK
                               (cancel 5MK TICK-RATE)
                               5HP
                               (cancel 5HP TICK-RATE)
                               fenrir))
