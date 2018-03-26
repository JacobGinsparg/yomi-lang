#lang racket

(require (for-syntax syntax/parse)
         "../lib/mock-device.rkt")
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

(define-syntax (check-received-inputs stx)
  (syntax-parse stx
    [(_ expected action) #'(with-mock-device
                             (perform-move action)
                             (check-equal expected received-inputs))]))