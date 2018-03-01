Milestones: MVP
---------------

This is what we aim to accomplish over the course of the semester.

1. Implement a Racket library to fire keyboard events on Linux-based operating systems. (This will likely require use of Racket's FFI to interface with the `uinput` kernel module.)

2. Extend the library to allow for sequences of keyboard events to be fired with specific timings.

3. Implement game schemas, allowing for buttons and direction identifiers to be used in sequences instead of literal key presses. Define a schema for Skullgirls, our target game.

4. Implement character schemas, allowing for a character-specific moves and frame data properties to be defined. Define a schema for Filia, one of the characters in Skullgirls.

5. Implement basic combos in the Yomi eDSL that can be written using constructs defined in game/character schemas. Combos should be able to be executed by the base Racket library. (At this point in time, we are not concerned about getting input timings working.)

6. Improve the Yomi combo eDSL to support links and cancels, so inputs can be performed with specific timings.

Milestones: Next steps
----------------------

Once the above milestones are reached, we want to extend compatibility so Yomi can be used on other platforms. We need to update the core Racket library to be able to fire keyboard events on other platforms. Our main priority will be Windows, as the overwhelming majority of fighting games get released for that platform. After that, we can look at supporting macOS.