# Understanding schemas

A complete Yomi program is comprised of three layers: a game schema, a character schema, and combos. Combos depend on the character schema, and the character schema depends on the game schema.


## Game schemas

A game schema is a basic description of a game. Game schemas are defined using `define-game`. The first argument is the name of the game, followed by clauses for buttons and tick rate.

The `buttons` clause specifies how many buttons the game has and what their labels are. A game may have up to eight buttons. Button labels must only be comprised of alphabetic characters and they must be unique. These labels are referenced in moves.

The `tick-rate` clause specifies the frequency at which the game's state updates, measured in frames per second. Yomi needs to know this in order to properly time inputs based on frame data. This should not be confused with the rate at which new images are drawn on the screen, often called the frame rate. If you're unsure of a game's tick rate, it's probably sixty.

Only one game schema may be defined per file and per program.

Here is an example game schema for Skullgirls:
```racket
(define-game skullgirls
  [buttons LP MP HP LK MK HK]
  [tick-rate 60])
```

And here is another one for Blazblue:
```racket
(define-game blazblue
  [buttons A B C D]
  [tick-rate 60])
```


## Character schemas

A character schema describes a character's moveset and their frame data properties. Character schemas are defined using `define-character`. The first argument is the character's name, followed by any number of move definitions.

Only one character schema may be defined per file and per program. A game schema must be present before defining a character schema. You may define the two schemas in the same file, or you may load the game schema from another file with `using-game`.

Moves are defined using the `move` keyword. The first argument is the move's input sequence; see [Writing moves](moves.md) for more details. The next four arguments are the move's frame data: startup, active frames, hitstun, and recovery. These must all be natural numbers.

At the bottom of a character schema (after the moves), you can define aliases that allow you to reference moves in combos by names instead of their inputs. This is done using the `alias` keyword.

Suppose that the Skullgirls game schema resides in the file "skullgirls.rkt" and we want to create a character schema for Filia in the same directory. The file "filia.rkt" would look like this:
```racket
(using-game "skullgirls.rkt")

(define-character filia
  (move 5LP 5 3 7 14)
  (move 5MP 13 3 9 19)
  ; And so on...
  (move 5HP 12 3 11 21)
  (alias launcher 5HP))
```


## Writing combos

Combos can be defined using `define-combo`. A combo begins with its name, followed by the sequence of actions. You can only use moves or aliases defined in the character schema, separated by links (`&`) or cancels (`~`).

A character schema must be present in order to define combos. You can either define combos in the same file as the character schema, or load the character schema from another file with `using-character`.

```racket
(using-character "filia.rkt")

(define-combo simple
  5LP ~ 5MP ~ launcher)
```