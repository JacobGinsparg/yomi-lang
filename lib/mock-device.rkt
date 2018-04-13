; Mock virtual device for testing and unsupported platforms.

#lang racket

(require (for-syntax syntax/parse))

(provide setup
         press
         hold
         release
         teardown
         received-inputs
         is-set-up
         (rename-out [my-make-event make-event])
         event-state
         event-inputs
         my-perform-combo
         write-input-event)


; fired-inputs is a list of InputEvent that can be viewed for testing purposes.
; Newer events are pushed to the end of the list.
(define received-inputs '())
(define is-set-up #f)

; An Event is a (make-event State [Listof Input] Number)
; Signifies that all of the Inputs are in a State simultaneously for some amount
; of time, measured in seconds.
; (It doesn't make sense for a delay Event to have Inputs, but we don't enforce
; that since this is just for development/testing.)
(define-struct event (state inputs duration) #:transparent)
(define-syntax my-make-event
  (syntax-parser
    [(_ state inputs) #'(make-event state inputs 0)]
    [(_ state inputs duration) #'(make-event state inputs duration)]
    [else #'make-event]))

; A State is one of:
; - 'hold
; - 'release
; - 'delay

(define (write-input-event type inputs duration)
  (set! received-inputs (append received-inputs (list (make-event type inputs duration)))))

(define (setup)
  (set! is-set-up #t))

(define (press . inputs)
  (error-if-uninitialized 'press)
  (write-input-event 'hold inputs 0)
  (write-input-event 'release inputs 0))

(define (hold . inputs)
  (error-if-uninitialized 'hold)
  (write-input-event 'hold inputs 0))

(define (release . inputs)
  (error-if-uninitialized 'release)
  (write-input-event 'release inputs 0))

(define (teardown)
  (error-if-uninitialized 'teardown)
  (set! received-inputs '())
  (set! is-set-up #f))

(define (error-if-uninitialized sym)
  (when (not is-set-up)
    (error sym "device not initialized; run setup")))
