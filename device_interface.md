Yomi Devices
============

Combos written in the Yomi DSL can be executed on a virtual input device. The
following specifies the API for Yomi devices, so you can understand how to
implement your own.

Inputs
------

A Yomi device must accept ten different button inputs, plus four primitive
directional inputs. (Recall that diagonal directions are a combination of two
primitive directions).

```
An Input is one of:
- Button
- Direction

A Button is one of the following Symbols:
- 'b1
- 'b2
- 'b3
- 'b4
- 'b5
- 'b6
- 'b7
- 'b8
- 'start
- 'select

A Direction is one of the following Symbols:
- 'up
- 'down
- 'back
- 'forward
```

The intention is that buttons `'b1` through `'b8` will be used for in-game
actions (i.e. six attacks plus two macros), while `'start` and `'select` will
be used for UI functions like pausing, resetting save states, etc.

Note that directions are relative to the player 1 side. In numpad notation,
back is 4 and forward is 6.


Functions
---------

A Yomi device must implement the following functions in order to conform to the
library's interface:

```
setup: Void -> Void
Perform any steps needed to initialize the device so it can perform inputs.

press: . Input -> Void
Instantaneously hold and release the given Inputs.

hold: . Input -> Void
Start holding all of the given Inputs.

release: . Input -> Void
Release all of the given inputs.

teardown: Void -> Void
Perform any steps needed to cleanly shut down the device.
```
