#lang racket

(require "helpers.rkt")
(require "../lib/schemas.rkt")

; These can't go in the game/character test files because those have schemas.

(check-def-failure (#%plain-module-begin (using-game "helpers.rkt"))
                   "Game schema not defined")

(check-def-failure (#%plain-module-begin (using-character "helpers.rkt"))
                   "Character schema not defined")