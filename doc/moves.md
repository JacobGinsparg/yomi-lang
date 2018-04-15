# Writing moves

A move is a single action in a game that can be triggered by performing a sequence of directions and buttons. A move's identifier is is the input sequence. Below is the grammar for writing moves. Note that it includes the use of angle brackets, which are normally used in BNFs to refer to other rules and types. To avoid confusion we are using curly braces to refer to rules and types.

```
Prefix ::= {Symbol}.

Direction ::= {1-9}

DirectionSeq ::= {Direction}[<{Nat}>]
                | {Direction}[<{Nat}>]{DirectionSeq}

Button ::= {Symbol}

ButtonSeq ::= {Button}[<{Nat}>]
            | {Button}+{ButtonSeq}

Move ::= [{Prefix}][{DirectionSeq}]{ButtonSeq}
```

## Prefixes

The _prefix_ is an optional piece at the beginning of the move used to differentiate it from other moves with the same inputs. For example, it is possible that a move in the air is performed with the same input sequence as a move on the ground, but has different frame properties. A prefix for aerial moves would allow the programmer to distinguish between these two moves.

Prefixes do not need to be declared in a schema before they can be used.

## Directions

A _direction_ is a digit in the range [1, 9] that signifies a directional input. Directions are written in "numpad notation", meaning that each digit is equivalent to its position on a keyboard's number pad:

```
7 8 9     ↖ ↑ ↗
4 5 6     ←   →
1 2 3     ↙ ↓ ↘ 
```

Note that directions are written such that the character is on the left side of the screen and facing right. This means 8 is up, 2 is down, 4 is backward, and 6 is forward. 5 is the neutral position, equivlant to inputting no direction. 1, 3, 7, and 9 are used for diagonals.

In a combo, the _direction sequence_ is an optional sequence of directions that will be performed one at a time in order from left to right. If a direction needs to be held for some interval before inputting the next direction, the length of the interval (in frames) can be specified by wrapping it in parenthesis after the individual direction. If the direction sequence is not specified, we infer that the direction is 5 (neutral/none).

## Buttons

_Buttons_ are labels defined in the game schema.

In a combo, the _button sequence_ is a combination of buttons that will be performed simultaneously. Each button must be separated by a +. If the button combination must be held for some interval, the interval can be specified (in frames) by wrapping it parenthesis after the whole button sequence.

## Examples

- `j.236K`: Assumed to be in the air, input directions down, down-forward, and forward, then press the K button
- `623HP+HK`: Input directions forward, down, and down-forward, then press the HP and HP buttons simultaneously
- `4<30>6HP`: Input direction back for 30 frames, then input direction forward and press the HP buton
- `j.HK`: Assumed to be in the air, press the HK button (no direction, equivalent to "j.5HK")