#lang racket

(require (for-syntax syntax/parse
                     rackunit)
         "../lib/mock-device.rkt"
         "../lib/yomi-lib.rkt"
         rackunit)
(provide ensure-setup
         ensure-teardown
         with-mock-device
         check-received-inputs
         check-def-failure)


; Safely setup mock device
(define (ensure-setup)
  (when is-set-up (teardown))
  (setup))

; Safely teardown mock device
(define (ensure-teardown)
  (when is-set-up (teardown)))

; Wrap expressions in mock device setup/teardown
(define-syntax (with-mock-device stx)
  (syntax-parse stx
    [(_ stuff ...) #'(begin (ensure-setup)
                            stuff ...
                            (ensure-teardown))]))

; Perform a move and check that its inputs are correct
(define (check-received-inputs expected action)
  (with-mock-device
    (perform-move action)
    (check-equal? received-inputs expected)))


; Expand the macro and assert that it fails with the given message
(define-syntax check-def-failure
  (syntax-parser
    [(_ def msg)
     (check-exn (regexp (syntax->datum #'msg))
                (lambda () (local-expand #'def 'module-begin null)))
     #'(void)]))