#lang racket

(require "../../lib/schemas.rkt")


; Skullgirls game schema

(define-game skullgirls
  [buttons LP MP HP LK MK HK]
  [tick-rate 60])