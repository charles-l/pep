# pep
A minimal orthogonal Vim-like clone.

## Current features
* Basic editing tasks
* ~600 SLOC (goal is to stay under 8K - less if possible)

## Design
* Motions
    * Motions just move the cursor from one place to another. They're prefixed with an `m_`
    * Motions are easy to add. They just require the current buffer.
* Edits
    * Edits track the difference between two motions and perform some transformation on the inbetween

## TODO
* Have config header
* Make keybindings easier to configure
