# Combos

A combo is a sequence of moves performed with specific timings. Combos are defined using the `define-combo` keyword. A combo begins with its name, followed by its moves (defined in the character schema), which are separated by links/cancels.

A link is denoted by `&`. This will cause the next move to be performed at the end of the current move's recovery.

A cancel is denoted by `~`. This will cause the next move to be performed at the end of the current move's active/hitstun, skipping the recovery.

Combos can be defined in the same file as the character schema, but this is discouraged. Instead, combos should be defined in a separate file and the schema should be loaded using `using-character`. Multiple combos can be defined in a single file, though their names must be unique.


## Example

The [schema documentation](schemas.md) shows the creation of a game schema in "skullgirls.rkt" and a character schema in "filia.rkt". We are going to create a new file in the same directory, called "combos.rkt". Then, we will load the character schema (the game schema is loaded implicitly).

```racket
(using-character "filia.rkt")

(define-combo basic-launcher
  5LP ~ 5LP ~ 5LK ~ 5MP ~ 5HP)

; Note that we need to link 5HK into 2LP. A cancel won't work.
(define-combo basic-otg-fenrir
  5HK & 2LP ~ 5MK ~ 5HP ~ 623MP+HP)
```
