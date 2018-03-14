There are a few gotchas to be aware of when using the uinput device:

- The `uinput` kernel module must be loaded.
  (Quick and dirty fix: `modprobe uinput`)
- Your user must have permission to write to `/dev/uinput`.
  (Quick and dirty fix: `chmod 0666 /dev/uinput`)
- Any sort of "repeat key" functionality must be disabled, or else held inputs
  will not work properly. Fixing this depends on your desktop environment and
  display server. For GNOME on X, I disabled "Repeat Keys" under Universal
  Access > Typing.