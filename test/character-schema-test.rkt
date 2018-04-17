#lang racket

(require rackunit
         "../lib/yomi-lib.rkt"
         "../lib/mock-device.rkt"
         "../lib/schemas.rkt"
         "game-schema-test.rkt"  ; we use the game schema defined in here
         "helpers.rkt")
(require (for-syntax syntax/parse))

; Happy cases

; Syntax is totally subject to change! Not sure how to set the game right now.
(define-character somebody
  (move HP 1 2 3 4)                  ; One button
  (move MP+HP 1 2 3 4)               ; Many buttons
  (move 2HP 1 2 3 4)                 ; One direction, one button
  (move 2HP+HK 1 2 3 4)              ; One direction, many buttons
  (move 236HP 1 2 3 4)               ; Many directions, one button
  (move 236HP+HK 1 2 3 4)           ; Many directions, many buttons
  (move 623HP 1 2 3 4))
;(move 2HP+HK<30> 1 2 3 4)          ; Held buttons
;(move 4<30>6HP 1 2 3 4)            ; Held direction (one)
;(move 2<10>3<20>6HP 1 2 3 4)       ; Held direction (many)
;(move 2<10>3<20>6HP<30> 1 2 3 4)   ; Held buttons and directions

; Wrap in #%module-begin so this runs at the same level as some-character on
; line 14
(check-def-failure (#%module-begin
                    (define-character somebody-else))
                   "Character schema already defined")

; note that the last direction and the first button must be performed together
(check-received-inputs (list (make-event 'hold (list button:HP))
                             (make-event 'release (list button:HP)))
                       HP)
(check-received-inputs (list (make-event 'hold (list button:MP button:HP))
                             (make-event 'release (list button:MP button:HP)))
                       MP+HP)
(check-received-inputs (list (make-event 'hold (list 'down button:HP))
                             (make-event 'release (list 'down button:HP)))
                       2HP)
(check-received-inputs (list (make-event 'hold (list 'down button:HP button:HK))
                             (make-event 'release (list 'down button:HP button:HK)))
                       2HP+HK)
(check-received-inputs (list (make-event 'hold (list 'down))
                             (make-event 'release (list 'down))
                             (make-event 'hold (list 'down 'forward))
                             (make-event 'release (list 'down 'forward))
                             (make-event 'hold (list 'forward button:HP))
                             (make-event 'release (list 'forward button:HP)))
                       236HP)
(check-received-inputs (list (make-event 'hold (list 'down))
                             (make-event 'release (list 'down))
                             (make-event 'hold (list 'down 'forward))
                             (make-event 'release (list 'down 'forward))
                             (make-event 'hold (list 'forward button:HP button:HK))
                             (make-event 'release (list 'forward button:HP button:HK)))
                       236HP+HK)
(check-received-inputs (list (make-event 'hold '(forward))
                             (make-event 'release '(forward))
                             (make-event 'hold '(down))
                             (make-event 'release '(down))
                             (make-event 'hold (list 'down 'forward button:HP))
                             (make-event 'release (list 'down 'forward button:HP)))
                       623HP)
; TODO
; - Hold tests (what's the best way to check for holds in received-inputs?)
; - Prefix tests
; Error cases, see below


; Error cases

; DirectionSeq only
; ButtonSeq with many holds
; Holds in the wrong place
; Undefined buttons
; Missing plus signs