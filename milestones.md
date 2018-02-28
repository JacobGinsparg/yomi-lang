Milestones: MVP
---------------

Implement a core Racket library to fire individual keyboard events on Linux-based operating systems. (This will likely require use of Racket's FFI to interface with the `uinput` kernel module.)

Implement a notion of combos in the Racket library, allowing for sequences of keyboard events to be fired with specific timings.

Implement game schemas. Allow buttons and directions defined in the Yomi language to resolve to keyboard events in the Racket library. Define a schema for Skullgirls.

Implement basic combos in the Yomi language using game schemas. Allow combos written using Yomi buttons and directions to resolve to a sequence of keyboard events in the Racket library. (Thes e inpiuts will likely be performed at a fixed interval, since frame data is not defined in the game schema.)
 
Implement character schemas where a character's moves and frame data properties can be defined. Define a schema for Filia.

Update Yomi combos to use character schemas and infer timings based on defined frame data properties.


Milestones: Next steps
----------------------

Update the core Racket library to fire keyboard events on other platforms. First priority is Windows, followed by macOS.