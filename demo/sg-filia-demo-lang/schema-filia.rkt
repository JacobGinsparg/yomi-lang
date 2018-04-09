#lang racket

(require "../../lib/yomi-lang.rkt")
(using-game "schema-skullgirls.rkt")


; Filia character schema
; (this is incomplete; just enough for simple testing)
(define-character filia
  (move 5LP 5 3 7 14)
  (move 5MP 13 3 9 19)
  (move 5HP 12 3 11 21)
  (move 5LK 9 3 7 19)
  (move 5MK 10 3 9 17)
  (move 5HK 19 9 14 26)
  (move 2LP 6 3 7 7)
  (move 623MP+HP 4 33 34 75)) ; expect these numbers to be inaccurate

(provide fenrir)
(define fenrir 623MP+HP)