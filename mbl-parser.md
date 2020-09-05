# Understanding MBL parser

To parse a string to it's `.mbl` format, there are a couple of intermediate steps.
This document tries to explain them.

## MBL
A `String` will eventually be transformed in a list of `MBL`*. Each of which is a list of actions.
Each `MBL` is itself "playable" (with `Mbl.interpret`, so things like the override repeat strategy from the CLI should be baked in).

*the list of `MBLs` is only useful to inspect a file content, as it will always choose a single `MBL` with the `lane` configuration.

## Core
The raw string gets interpreted, line by line (skipping whitespace) to `Core`. 
A `Core` line can be:

  1. A candidate MBL (either an un-refed string*, or a wait with no notion of the tick rate).
  1. A ref waiting to be bound to the corresponding MBL print
  1. A `tick` statement

_*a note on un-refed: A single MBL core candidate can be multiple actions, if a ref matches partially._

Once a file is fully read, it gets bound into an `IntermediateMBL` that is solely for ease of use.
After this refs get bound in into the `MBL` list.
And finlay, the overridden configurations get baked in.