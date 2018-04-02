#lang racket

(require rackunit
         "../lib/yomi-lib.rkt"
         "../lib/mock-device.rkt")
(require (for-syntax syntax/parse))

; Happy cases

(define-game some-game
  [buttons LP MP HP LK MK HK]
  [tick-rate 60])

; Syntax is totally subject to change! Not sure how to set the game right now.
(define-character some-character some-game
  (move HP . . . .)                  ; One button
  (move MP+HP . . . .)               ; Many buttons
  (move 2HP . . . .)                 ; One direction, one button
  (move 2HP+HK . . . .)              ; One direction, many buttons
  (move 236HP . . . .)               ; Many directions, one button
  (move 236HP+HK . . . .)            ; Many directions, many buttons
  (move 2HP+HK<30> . . . .)          ; Held buttons
  (move 4<30>6HP . . . .)            ; Held direction (one)
  (move 2<10>3<20>6HP . . . .)       ; Held direction (many)
  (move 2<10>3<20>6HP<30> . . . .))  ; Held buttons and directions
(require (submod "." some-character))

; note that the last direction and the first button are performed separately
(check-received-inputs (list (make-event 'hold (list HP))
                             (make-event 'release '(list HP)))
                       HP)
(check-received-inputs (list (make-event 'hold (list MP HP))
                             (make-event 'release (list MP HP)))
                       MP+HP)
(check-received-inputs (list (make-event 'hold (list 'down))
                             (make-event 'release (list 'down))
                             (make-event 'hold (list HP))
                             (make-event 'release (list HP)))
                       2HP)
(check-received-inputs (list (make-event 'hold (list 'down))
                             (make-event 'release (list 'down))
                             (make-event 'hold (list HP HK))
                             (make-event 'release (list HP HK)))
                       2HP+HK)
(check-received-inputs (list (make-event 'hold (list 'down))
                             (make-event 'release (list 'down))
                             (make-event 'hold (list 'down 'forward))
                             (make-event 'release (list 'down forward))
                             (make-event 'hold (list 'forward))
                             (make-event 'release (list 'forward))
                             (make-event 'hold (list HP))
                             (make-event 'release (list HP)))
                       236HP)
(check-received-inputs (list (make-event 'hold (list 'down))
                             (make-event 'release (list 'down))
                             (make-event 'hold (list 'down 'forward))
                             (make-event 'release (list 'down forward))
                             (make-event 'hold (list 'forward))
                             (make-event 'release (list 'forward))
                             (make-event 'hold (list HP HK))
                             (make-event 'release (list HP HK)))
                       236HP+HK)
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