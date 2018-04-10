#lang racket

(require "../../lib/yomi-lang.rkt")
(using-character "schema-filia.rkt")


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