#lang racket

(require rackunit
         "helpers.rkt"
         "../lib/mock-device.rkt")

(test-case
 "setup"
 (ensure-teardown)
 (setup)
 (check-true is-set-up "should set is-set-up to #t"))

(test-case
 "teardown"
 (ensure-teardown)
 (check-exn exn:fail? teardown "should error if not set up yet")
 (setup)
 (hold 'b1)
 (check-equal? (length received-inputs) 1)
 (teardown)
 (check-equal? received-inputs '() "should clear received-inputs")
 (check-false is-set-up "should set is-set-up to #f"))

(test-case
 "single presses"
 (with-mock-device
   (press 'b1)
   (check-equal? (make-event 'hold '(b1)) (first received-inputs)
                 "fires a hold event")
   (check-equal? (make-event 'release '(b1)) (last received-inputs)
                 "fires a release event immediately after the hold")))

(test-case
 "multiple presses"
 (with-mock-device
   (press 'b1 'b2)
   (check-equal? (make-event 'hold '(b1 b2)) (first received-inputs)
                 "fires a hold event")
   (check-equal? (make-event 'release '(b1 b2)) (last received-inputs)
                 "fires a release event immediately after the hold")))

(test-case
 "single holds"
 (with-mock-device
   (hold 'b1)
   (check-equal? (make-event 'hold '(b1)) (first received-inputs)
                 "fires a hold event")))

(test-case
 "multiple holds"
 (with-mock-device
   (hold 'b1 'b2)
   (check-equal? (make-event 'hold '(b1 b2)) (first received-inputs)
                 "fires a hold event")))

(test-case
 "single release"
 (with-mock-device
   (release 'b1)
   (check-equal? (make-event 'release '(b1)) (first received-inputs)
                 "fires a release event")))

(test-case
 "multiple releases"
 (with-mock-device
   (release 'b1 'b2)
   (check-equal? (make-event 'release '(b1 b2)) (first received-inputs)
                 "fires a release event")))