# Understanding schemas

A complete Yomi program is comprised of three layers: a game schema, a character schema, and combos. Combos depend on the character schema, and the character schema depends on the game schema.


## Game schemas

A game schema is a basic description of a game. Game schemas are defined using `define-game`. The first argument is the name of the game, followed by clauses for buttons and tick rate.

The `buttons` clause specifies how many buttons the game has and what their labels are. A game may have up to eight buttons. Button labels must only be comprised of alphabetic characters and they must be unique. These labels are referenced in moves.

The `tick-rate` clause specifies the frequency at which the game's state updates, measured in frames per second. Yomi needs to know this in order to properly time inputs based on frame data. This should not be confused with the rate at which new images are drawn on the screen, often called the frame rate. If you're unsure of a game's tick rate, it's probably sixty.

Only one game schema may be defined per file and per program. The game schema must be defined in a separate file from the character schema.

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

A game schema needs to be loaded using `using-game` before defining a character schema. Only one character schema may be defined per file and per program. The character schema must be defined in a separate file from the game schema.

Moves are defined using the `move` keyword. The first argument is the move's input sequence; see "Writing moves and combos" for more details. The next four arguments are the move's frame data: startup, active frames, hitstun, and recovery. These must all be natural numbers.

Suppose that the Skullgirls game schema resides in the file "skullgirls.rkt" and we want to create a character schema for Filia in the same directory. The file "filia.rkt" would look like this:
```racket
(using-game "skullgirls.rkt")

(define-character filia
  (move 5LP 5 3 7 14)
  (move 5MP 13 3 9 19)
  ; And so on...
  (move 5HP 12 3 11 21))
```