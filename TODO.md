# Lexer

* [ ] Tok::Symbol
* [ ] Fixity

# AST

* [ ] Primop representation
* [ ] Visitor, Walker etc.

# Flatten

* [ ] Remove magic: closures should be constructed of a bunch of code object +
      clover tuple combinations and created and accessed as such
* [ ] Blocks should not close over variables
* [ ] Detect access to uninitialized variables (in blocks)
    - Doing this statically simplifies register allocation since we don't need
      to keep null references in registers between block start and variable
      definition just for error detection

# CPS IR

* [x] Trivial vs. nontrivial expressions
* [x] If
* [x] Next
* [x] Print conts topologically

# CPS Conversion

* [x] Clause condition treatment
    - [x] Use `Next`
* [ ] Break down closure initialization and clover references
