# Lexer

* [ ] Tok::Symbol
* [ ] Fixity

# AST

* [ ] Primop representation
* [ ] Visitor, Walker etc.

# Flatten

* [ ] Blocks should not close over variables
* [ ] Detect access to uninitialized variables (in blocks)
    - Doing this statically simplifies register allocation since we don't need
      to keep null references in registers between block start and variable
      definition just for error detection

# CPS IR

* [ ] Trivial vs. nontrivial expressions
* [ ] Print conts topologically

# CPS Conversion

* [ ] Break down closure initialization and clover references
* [ ] Clause condition treatment
