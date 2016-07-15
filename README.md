# pep
A minimal orthogonal Vim-like clone.

## Current features
* Basic editing tasks
    * Insert mode
    * Delete, join text and lines
    * Undo (partially implemented)
    * Read file
    * Write file
    * Basic piping out (with `|` to read from pipe or `!` to filter the entire buffer through command)
    * Searching (by piping out to `grep`)
* ~1000 SLOC (goal is to stay under 2K - less if possible)

## Design
* There's a buffer
* The buffer has a linked-list of lines
* Motions
    * Motions just move the cursor from one place to another. In code they're prefixed with an `m_`
* Edits
    * Edits track the difference between two motions and perform some transformation on the inbetween

## TODO
* Make usable (i.e. fix crashes...)
* Have config header
* Make keybindings easier to configure
* Make extensibility easier
