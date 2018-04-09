#lang racket

(require "../lib/schemas.rkt"
         "helpers.rkt")

(using-character "character-schema-test.rkt")
(using-game "game-schema-test.rkt")

(check-def-failure (#%module-begin (using-game "doesn't matter"))
                   "Already using game schema")

(check-def-failure (#%module-begin (using-character "doesn't matter"))
                   "Already using character schema")