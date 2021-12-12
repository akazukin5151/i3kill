# i3kill

I use tabbed layout exclusively on i3wm. The default `kill` command will kill a window and shift the focus to the window on the right. I want it to focus left instead, but then focus right if the leftmost window is the kill target (otherwise it would loop to focus on the rightmost window)

It runs shell commands using i3-msg, but is actually quite fast and indistinguishable from the native `kill` command

Multi-monitor setups might need patches

## Installation

- Haskell: `stack build && stack install`
- Python: copy `i3kill.py` to somewhere, like your PATH

## Usage

Put something like this in your i3 config:

`bindysm $mod+q exec i3kill`

If you're using the python script, write:

`bindysm $mod+q exec 'python /path/to/your/script/'`

Be advised that if you run it manually it will close your window. Duh

## Background

Written in Haskell because it is statically compiled and will not break. Once upon a time, one of my Python script crashed because it was running without conda for some reason (I wrote it inside the conda environment). Never again.

I did prototype with it using `jq` and Python though. So here's the Python script, use it at your own risk. Admittedly, it's only 20 lines long and only depends on the standard library so there should be no risk, but Python gave me PTSD so it's best to build up the reflexes to avoid using it for anything but prototyping.
