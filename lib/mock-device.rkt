; Mock virtual device for testing and unsupported platforms.

#lang racket

(provide setup
         press
         hold
         release
         teardown
         received-inputs
         is-set-up
         make-event
         event-state
         event-inputs)


; fired-inputs is a list of InputEvent that can be viewed for testing purposes.
; Newer events are pushed to the end of the list.
(define received-inputs '())
(define is-set-up #f)

; An Event is a (cons State [Listof Input])
; Signifies that all of the Inputs are being held/released simultaneously
(define make-event cons)
(define event-state first)
(define event-inputs rest)

; A State is one of:
; - 'hold
; - 'release

(define (setup)
  (set! is-set-up #t))

(define (press . inputs)
  (error-if-uninitialized 'press)
  (set! received-inputs (append received-inputs (list (make-event 'hold inputs)
                                                (make-event 'release inputs)))))

(define (hold . inputs)
  (error-if-uninitialized 'hold)
  (set! received-inputs (append received-inputs (list (make-event 'hold inputs)))))

(define (release . inputs)
  (error-if-uninitialized 'release)
  (set! received-inputs (append received-inputs (list (make-event 'release inputs)))))

(define (teardown)
  (error-if-uninitialized 'teardown)
  (set! received-inputs '())
  (set! is-set-up #f))

(define (error-if-uninitialized sym)
  (when (not is-set-up)
    (error sym "device not initialized; run setup")))