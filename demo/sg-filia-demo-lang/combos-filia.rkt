#lang racket

(require "../../lib/yomi-lang.rkt")

(using-character "schema-filia.rkt")

; Some combos! Finally!

(define-combo basic-launcher LP ~ LP ~ LK ~ MP ~ HP)

(define-combo basic-otg-fenrir HK & 2LP ~ MK ~ HP ~ fenrir)

(define-combo foo
  LP ~ LP ~ LK ~ MP ~ 2MK ~ HP
  ~ 214MP+HP & 214MK+HK & 2LP ~ MK ~ HP ~ 623MP+HP)

(wait-before-perform 3)

#;(define (go)
  (setup)
  (sleep 3)
  (perform-combo foo)
  (teardown))
  