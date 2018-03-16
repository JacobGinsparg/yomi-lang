; Demonstration of how to use Yomi at the library level.

#|
You'll notice a lot of patterns and repetitions in this code. That's sort of
intentional; I hope that it surfaces areas for improvement that we can solve at
the schema/language level.
|#

#lang racket

(require "../lib/yomi-lib.rkt"
         "../lib/uinput.rkt")


; Skullgirls game schema

(define LP 'b1)
(define MP 'b2)
(define HP 'b3)
(define LK 'b4)
(define MK 'b5)
(define HK 'b6)

(define TICK-RATE 60)

; ------------------------------------------------------------------------------

; Filia character schema
; (this is incomplete; just enough for simple testing)

(define 5LP (make-move (lambda () (press LP))
                       5 3 7 14))
(define 5MP (make-move (lambda () (press MP))
                       13 3 9 19))
(define 5HP (make-move (lambda () (press HP))
                       12 3 11 21))
(define 5LK (make-move (lambda () (press LK))
                       9 3 7 19))
(define 5MK (make-move (lambda () (press MK))
                       10 3 9 17))
(define 5HK (make-move (lambda () (press HK))
                       19 9 14 26))
(define 2LP (make-move (lambda () (press 'down LP))
                       6 3 7 7))
(define 623MP+HP (make-move (lambda () (begin (press 'forward)
                                              (press 'down)
                                              (press 'down 'forward)
                                              (press MP HP)))
                            4 33 34 75)) ; expect these numbers to be inaccurate
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
