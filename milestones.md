Milestones: MVP
---------------

Implement a Racket library to fire individual keyboard events on Linux-based operating systems. (This will likely require use of Racket's FFI to interface with the `uinput` kernel module.)

Extend the library to allow for sequences of keyboard events to be fired with specific timings.

Implement game schemas, allowing for buttons and directions to be used in input sequences instead of literal key presses. Define a schema for Skullgirls, our target game.

Implement character schemas where a character's moves and frame data properties can be defined. Define a schema for Filia (a playable character in Skullgirls).

Implement basic combos in the Yomi eDSL that can be written using constructs defined in game/character schemas. Combos should be able to be executed by the base Racket library. (At this point in time, we are not concerned about getting input timings working.)

Improve the Yomi combo eDSL to support links and cancels, so inputs can be performed with specific timings.

Milestones: Next steps
----------------------

Update the core Racket library to fire keyboard events on other platforms. First priority is Windows, followed by macOS.