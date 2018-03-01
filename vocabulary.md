Vocabulary
----------

A _direction_ is a digit in the range [1, 9] that corresponds to a directional input in the game. Directions are written using "numpad notation", meaning that each digit's directional input is equivalent to its position on a keyboard's number pad:

```
7 8 9
4 5 6
1 2 3
```

Note that in fighting games, directions are relative based on which way your character is facing. We do not say "left" and "right", we say "back" and "forward". Combos are written assuming that your character is on the left side of the screen and facing right, which follows existing conventions.

(8 is up, 2 is down, 4 is back, 6 is forward, 5 is neutral/none, others are diagonals)

A _button_ is an identifier (written using only alphabetical characters) that corresponds to one of a game's basic attacks.

A _move_ is a single action in the game that can be triggered by performing a sequence of directions and buttons. A move has three states: startup, active, and recovery. The length of these states is measured in frames and must be defined for each move.

A _combo_ is a sequence of moves performed with specific timings.

A _name_ is a binding of an identifier to a move or combo.

A _game schema_ is a namespace that defines buttons for a specific game. Moves and names can also be defined in a game schema to be referenced in character schemas.

A _character schema_ is a namespace that defines moves and names for a specific character in a specific game. A character schema must be within the scope of a game schema.

A _link_ is when a move is performed after the previous move's recovery state completes. We infer that the move should be performed on the first frame after recovery, but the delay can be manually specified.

A _cancel_ is when a move is performed after the previous move's active state completes, but is still in recovery. We infer that the move should be performed on the first frame of recovery, but the delay can be manually specified.


Anatomy of a move
-----------------

A move has three pieces (in the following order): a prefix, a direction sequence, and a button sequence.

The _prefix_ is an optional piece at the beginning of the move used to differentiate it from other moves with the same inputs. For example, it is possible that a move in the air is performed with the same input sequence as a move on the ground, but has different frame properties. A prefix for aerial moves would allow the programmer to distinguish between these two moves.

The _direction sequence_ is an optional sequence of directions that will be performed one at a time in order from left to right. If a direction needs to be held for some interval before inputting the next direction, the length of the interval (in frames) can be specified by wrapping it in parenthesis after the individual direction. If the direction sequence is not specified, we infer that the direction is 5 (neutral/none).

The _button combination_ is a combination of buttons that will be performed simultaneously. Each button must be separated by a +. If the button combination must be held for some interval, the interval can be specified (in frames) by wrapping it parenthesis after the whole button sequence.

Some example moves:
- j.236K: Assumed to be in the air, input directions down, down-forward, and forward, then press the K button
- 623HP+HK: Input directions forward, down, and down-forward, then press the HP and HP buttons simultaneously
- 4(30)6HP: Input direction back for 30 frames, then input direction forward and press the HP buton
- j.HK: Assumed to be in the air, press the HK button (no direction, equivalent to "j.5HK")