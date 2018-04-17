# Performing combos using uinput

Yomi hooks into the uinput Linux kernel module to emulate a keyboard, allowing you to see your Yomi combos performed in games.

Support for non-Linux operating systems will be added in a future version of Yomi.


## Setup

Running `make uinput` from Yomi's directory should work, but just in case, the manual steps are:

1. Compile the C shared object by running `make lib/linux_uinput.so`.
2. Enable the kernel module by running `sudo modprobe uinput`.
3. Ensure you can write to `/dev/uinput` by running `sudo chmod +0666 /dev/uinput`.

Note that you will need to repeat these steps (or re-run the Make task) if you restart your system.

Once everything is set up, run `make drracket` to launch DrRacket with the correct `LD_LIBRARY_PATH`.


## Key bindings

You will need to make sure that your game's controls are bound to the keys used by Yomi. Yomi will bind the following keys to the buttons specified in the game schema in the order that the buttons appear (left to right):
```
Q W E R T Y U I
```

For example, the following game schema will bind button A to `Q`, button B to `W`, button C to `E`, and button D to `R`:
```racket
(define-game blazblue
  [buttons A B C D]
  [tick-rate 60])
```

Arrow keys are used for directional inputs.


## Performing combos

The `perform` function takes a move or a combo as input and executes it via uinput. By default, there is no delay between calling the function and seeing the action performed; it will begin immediately. You may want to add a delay so you have time to switch windows from your code to the game. This can be done using `wait-before-perform`, a function that consumes consumes a number and adds a delay (in seconds) to every subsequent `perform` call.

```racket
(define-combo some-combo
  ...)
(wait-before-perform 3)
(perform some-combo)  ; will wait 3 seconds before firing inputs
```