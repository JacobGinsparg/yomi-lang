#lang racket

(require (for-syntax rackunit
                     syntax/parse)
         rackunit
         "helpers.rkt"
         "game-schema-test.rkt"
         "../lib/mock-device.rkt"
         "../lib/schemas.rkt"
         "../lib/yomi-lib.rkt")

(define-syntax check-def-failure
  (syntax-parser
    [(_ def msg)
     (check-exn (regexp (syntax->datum #'msg))
                (lambda () (local-expand #'def 'module-begin null)))
     #'(void)]))

(using-character "character-schema-test.rkt")

(define-combo basic-link HP & HP)

(define-combo basic-cancel HP ~ HP)

(define-combo link-then-cancel HP & HP ~ HP)

(define-combo cancel-then-link HP ~ HP & HP)

(define-combo bad-combo +)

(check-combo-inputs (list (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP))
                          (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP)))
                    basic-link)

(check-combo-inputs (list (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP))
                          (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP)))
                    basic-cancel)

(check-combo-inputs (list (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP))
                          (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP))
                          (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP)))
                    link-then-cancel)

(check-combo-inputs (list (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP))
                          (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP))
                          (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP)))
                    cancel-then-link)

(test-case
 "Bad combos"
 
 (check-exn
  exn:fail?
  (lambda ()
    (check-combo-inputs (list (make-event 'hold (list button:HP))
                              (make-event 'release (list button:HP)))
                        bad-combo)))

 (check-def-failure (define-combo bad-chain HP + HP)
                    "Use ~ for cancels, & for links"))