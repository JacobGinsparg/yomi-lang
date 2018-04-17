# Yomi: A fighting game language

Yomi is a language for scripting inputs in fighting games. Its goal is to allow combos to be written using syntax that resembles conventional fighting game notation as closely as possible.


## Documentation table of contents
* [Understanding schemas](schemas.md)
* [Writing moves](moves.md)
* [Performing combos using uinput](uinput.md)


## Requirements and setup

Yomi is built in Racket. We recommend using DrRacket for development.

Yomi uses the uinput kernel module in order to simulate a keyboard. Therefore, it is only compatible with Linux at this time.

Run `make uinput` to ensure uinput is set up on your computer. Then, run `make drracket` to launch DrRacket with the correct path.


## A sample program

```racket
; First, define a game schema that specifies a game's buttons and tick rate.
(define-game skullgirls
  [buttons LP MP HP LK MK HK]
  [tick-rate 60])

; Next, define a character schema that specifes inputs and frame data. You can
; also bind moves to aliases to simplify writing your combos.
(define-character filia
  (move LP 5 3 7 14)
  (move MP 13 3 9 19)
  (move HP 12 3 11 21)
  (move 623MP+HP 4 34 35 74)
  (alias fenrir 623MP+HP))

; Finally, write some combos!
(define-combo launch-into-fenrir
  LP ~ MP ~ HP ~ fenrir)

; To perform it in-game:
(perform launch-into-fenrir)
```