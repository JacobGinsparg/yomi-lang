#lang racket

(require "schemas.rkt"
         "uinput.rkt"
         "yomi-lib.rkt")

(provide (all-from-out "schemas.rkt")
         (all-from-out "uinput.rkt")
         (all-from-out "yomi-lib.rkt")
         perform
         wait-before-perform)

(define sleep-for 0)

; wait-before-perform: Nat -> Void
; Set how long to wait before performing moves or combos (in seconds)
(define (wait-before-perform s)
  (set! sleep-for s))

; perform: (U Combo Move) -> Void
; Performs the given Combo or Move
(define (perform x)
  (setup)
  (sleep sleep-for)
  (cond [(list? x) (perform-combo x)
                   (teardown)]
        [(move? x) (perform-move x)
                   (teardown)]
        [else (teardown)
              (error 'perform "invalid argument")]))