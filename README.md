Haskell GPIO interface, designed specifically for the RaspberryPi.

This library also ships with an executable for directly running gpio commands.



Note: this package has limited functionality, might be buggy and is not optimized. It works for simple read/write operations on a few pre-defined pins, but still needs a lot more testing.

TODO:
  * Remove `runLineHack` when reading GPIO value files.
  * Optimize file reading to only look at first char.
  * Support other pin modes (in/out/up/down/pwm)
  * Support edges
  * Look into GPIO supported file watching
  * Add all pin numbers
  * Decide on a pin mapping strategy.
