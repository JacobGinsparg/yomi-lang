; Core of the Yomi library that executes inputs.

#|
List of problems I can think of with this implementation:
- Does not support direction sequences in combos, preventing charge buffers,
  jumping/moving, etc.
- Heavily tied to Linux-specific uinput code. We want something more like
  "require whatever module implements the virtual device interface for this
  platform".
- Constantly having to pass the tick rate to functions is a pain and I'm a bit
  uncertain how you'd write a move with charge inputs.
|#

#lang racket

; TODO: make this configurable and/or platform-specific
(require "uinput.rkt")
(provide make-move
         link
         cancel
         perform-combo
         perform-move
         setup
         teardown
         press
         hold
         release)


; A Move is a (make-move InputSeq Integer Integer Integer Integer)
; inputs should be the InputSeq that triggers the move
; startup, active, hitstun, recovery are frame data properties
(define-struct move (inputs startup active hitstun recovery))

; An InputSeq is a thunk function that triggers inputs when evaluated

; A Combo is a [Listof (U Move Delay)]

; A Delay is a thunk function that waits for some particular amount of time

; ------------------------------------------------------------------------------

; Functions for determining timings on moves

; frames-seconds: Integer Integer -> Number
; Based on the given tick rate, convert the given number of frames into seconds
(define (frames->seconds frames tick-rate)
  (/ frames tick-rate))

; link: Move Integer -> Delay
; Produces a thunk that will sleep until the end of the given move's recovery
(define (link m tick-rate)
  (lambda () (sleep (frames->seconds (+ (move-startup m)
                                        (move-hitstun m)
                                        (move-recovery m))
                                     tick-rate))))

; cancel: Move Integer -> Delay
; Produces a thunk that will sleep until the end of the given move's hitstun
(define (cancel m tick-rate)
  (lambda () (sleep (frames->seconds (+ (move-startup m)
                                        (move-hitstun m))
                                     tick-rate))))

; ------------------------------------------------------------------------------

; Functions that actually perform combos

; perform-combo: Combo -> Void
; Execute the given combo on the virtual input device.
(define (perform-combo combo)
  (cond [(empty? combo) (void)]
        [(move? (first combo)) (begin (perform-move (first combo))
                                      (perform-combo (rest combo)))]
        [else (begin ((first combo))
                     (perform-combo (rest combo)))]))

; perform-move: Move -> Void
; Execute the given move on the virtual input device.
(define (perform-move m)
  ((move-inputs m)))