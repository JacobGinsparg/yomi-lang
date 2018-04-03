#lang racket

(require (for-syntax syntax/parse)
         "../lib/mock-device.rkt"
         "../lib/yomi-lib.rkt"
         rackunit)
(provide ensure-setup
         ensure-teardown
         with-mock-device
         check-received-inputs)

(define (ensure-setup)
  (when is-set-up (teardown))
  (setup))

(define (ensure-teardown)
  (when is-set-up (teardown)))

(define-syntax (with-mock-device stx)
  (syntax-parse stx
    [(_ stuff ...) #'(begin (ensure-setup)
                            stuff ...
                            (ensure-teardown))]))

(define (check-received-inputs expected action)
  (with-mock-device
    (perform-move action)
    (check-equal? received-inputs expected)))