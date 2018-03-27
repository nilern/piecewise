* [x] Use `std::any::TypeId`
* [x] Make domain::Allocator global (using lazy_static and a singleton-y access function).
    - [x] => Replace Dynamic* traits with std Debug and Display
* [x] `method ::= pattern ('|' expr)? => expr`
* [x] Handwritten lexer.
* [x] Rewrite parser in direct style, with functions and Result.
* [ ] Rewrite pretty printing similarly to parsing, using extra context nesting and parent
      precedence arguments.
* [ ] Remove the vestigial cst and just parse to ast directly (need to add source positions to ast).
* [x] Use a contiguous mutable stack for top cont.
