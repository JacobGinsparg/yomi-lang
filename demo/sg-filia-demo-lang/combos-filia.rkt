#lang racket

(require "../../lib/uinput.rkt"
         "../../lib/yomi-lib.rkt"
         "../../lib/schemas.rkt")

(using-character "schema-filia.rkt")

; Some combos! Finally!

(define-combo basic-launcher 5LP ~ 5LP ~ 5LK ~ 5MP ~ 5HP)

(define-combo basic-otg-fenrir 5HK & 2LP ~ 5MK ~ 5HP ~ fenrir)
