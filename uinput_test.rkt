; Not a real test. Just a "look, it works".

#lang racket

(require "uinput.rkt")

(define (tumble)
  (sleep 3)
  (hold 'down 'left)
  (sleep 2)
  (release 'down 'left)
  (press 'right 'b6)
  (sleep 3/4)
  (press 'b3))