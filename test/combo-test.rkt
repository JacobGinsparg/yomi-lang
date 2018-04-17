#lang racket

(require (for-syntax rackunit
                     syntax/parse)
         rackunit
         "helpers.rkt"
         "game-schema-test.rkt"
         "../lib/mock-device.rkt"
         "../lib/schemas.rkt"
         "../lib/yomi-lib.rkt")

;; NOTE: Need to update tests to play nicer with holds when we add hold support

(using-character "character-schema-test.rkt")

(define-combo bad-combo    +)
(define-combo short-combo  HP)
(define-combo basic-link   HP & HP)
(define-combo basic-cancel HP ~ HP)
(define-combo link-then-cancel HP & HP ~ HP)
(define-combo cancel-then-link HP ~ HP & HP)
(define-combo complex-combo    HP & HP ~ 236HP+HK)

(check-combo-inputs (list (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP))
                          (make-event 'delay '() 8/60)
                          (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP)))
                    basic-link)

(check-combo-inputs (list (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP))
                          (make-event 'delay '() 4/60)
                          (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP)))
                    basic-cancel)

(check-combo-inputs (list (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP))
                          (make-event 'delay '() 8/60)
                          (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP))
                          (make-event 'delay '() 4/60)
                          (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP)))
                    link-then-cancel)

(check-combo-inputs (list (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP))
                          (make-event 'delay '() 4/60)
                          (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP))
                          (make-event 'delay '() 8/60)
                          (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP)))
                    cancel-then-link)

(check-combo-inputs (list (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP))
                          (make-event 'delay '() 8/60)
                          (make-event 'hold (list button:HP))
                          (make-event 'release (list button:HP))
                          (make-event 'delay '() 4/60)
                          (make-event 'hold (list 'down))
                          (make-event 'release (list 'down))
                          (make-event 'hold (list 'down 'forward))
                          (make-event 'release (list 'down 'forward))
                          (make-event 'hold (list 'forward button:HP button:HK))
                          (make-event 'release (list 'forward button:HP button:HK)))
                    complex-combo)

(test-case
 "Bad combos"
 
 (check-exn
  exn:fail?
  (lambda ()
    (check-combo-inputs (list (make-event 'hold (list button:HP))
                              (make-event 'release (list button:HP)))
                        bad-combo)))

 (check-def-failure (define-combo bad-chain HP + HP)
                    "Use ~ for cancels, & for links")

 (check-def-failure (define-combo bad-chain HP ~ HP ~)
                    "Malformed combo"))