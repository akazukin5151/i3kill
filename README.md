# i3utils

Some utility programs I use for i3wm tabbed layout, which I use exclusively.

- i3kill
- i3focus
- i3movetab

Single monitor only.

It runs shell commands using i3-msg, but is actually quite fast and indistinguishable from the native `kill` command

## General Installation

- Haskell: `stack build && stack install`
    - This installs all three programs to `~/.local/bin/`
- Python: copy the scripts in the `python` dir to somewhere

- Usage of python scripts in general: `bindysm $mod+q exec 'python /path/to/your/script/'`

## i3kill

- The default `kill` command will kill a window and shift the focus to the window on the right. 
- `i3kill` would kill the window and focus left
- But if the leftmost window is killed, it will focus right (otherwise it would loop to focus on the rightmost window)

- Be advised that if you run it manually it will close your window. Duh

- Example usage: (use appropriate path for python script)

`bindysm $mod+q exec i3kill`

## i3focus

- Use mod+number keys to focus tabs (instead of workspaces)
- mod+1 will focus the first tab, and so on
- mod+0 will always focus the last tab

- Example usage: (use appropriate path for python script)

```
bindsym $mod+1 exec "i3focus 1"
bindsym $mod+2 exec "i3focus 2"
bindsym $mod+3 exec "i3focus 3"
bindsym $mod+4 exec "i3focus 4"
bindsym $mod+5 exec "i3focus 5"
bindsym $mod+6 exec "i3focus 6"
bindsym $mod+7 exec "i3focus 7"
bindsym $mod+8 exec "i3focus 8"
bindsym $mod+9 exec "i3focus 9"
bindsym $mod+0 exec "i3focus 0"
```

- Python script is similar to https://gist.github.com/syl20bnr/6623972 but has no dependency on external libraries

## i3movetab

- Swap or move the current tab with the one on the left/right.
- Moving the last tab Right, will move it to the leftmost tab (instead of swapping with the first tab, because the first tab would become the last tab)
- Moving the first tab Left, will move it to the rightmost tab (instead of swapping with the last tab, because the last tab would become the first tab)

- The directions must be in Capital case (eg, `Left` and `Right`)

- Example usage (use appropriate path for python script)

```
bindysm $mod+Shift+h exec "i3movetab Left"
bindysm $mod+Shift+l exec "i3movetab Right"
```

## Background

Written in Haskell because it is statically compiled and will not break. Once upon a time, one of my Python script crashed because it was running without conda for some reason (I wrote it inside the conda environment). Never again.

I did prototype with it using `jq` and Python though. So the Python scripts are here, use it at your own risk. Admittedly, it's much shorter and only depends on the standard library so there should be no risk, but Python gave me PTSD so it's best to build up the reflexes to avoid using it for anything but prototyping.

