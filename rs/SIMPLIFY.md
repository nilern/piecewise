* [x] Use `std::any::TypeId`
* [ ] Make domain::Allocator global (using lazy_static and a singleton-y access function).
    - [ ] => Replace Dynamic* traits with std Debug and Display
* [ ] `method ::= pattern ('|' expr)? => expr`
* [ ] Remove the vestigial cst and just parse to ast directly (need to add source positions to ast).
* [ ] Rewrite parser in direct style, with functions and Result.
* [ ] Rewrite pretty printing similarly to parsing, using extra context nesting and parent
      precedence arguments.
