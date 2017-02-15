# Lexical Syntax

    @DELIMITER = ['"()[]{}]
    @SEPARATOR = [,;]
    @TERMINATOR = \s | @DELIMITER | @SEPARATOR
    @CONSTITUENT = [^@TERMINATOR]

    NUMBER = [\d] @CONSTITUENT*
    ID = [\p{Alphabetic}_@] @CONSTITUENT*
    OP = [^\d\p{Alphabetic}_@@TERMINATOR] @CONSTITUENT*
    CHAR = ' [^']+ '
    STRING = " [^"]* "

# Context-Free Syntax

    stmts = (<stmt> ';')* <stmt>

    expr = <stmt>

    stmt = <pmtz>
         | <def>
         | <infix>

    pmtz = <infix> '=>' <infix>

    def = <infix> '=' <infix>

    infix = <infix> OP <app>
          | <app>

    app = <simple> <simple>+

    simple = '{' <stmts> '}'
           | '(' <expr> ')'
           | <coll>
           | <atom>

    coll = '(' (<expr> ',')* <expr> ')'
         | '[' (<expr> ',')* <expr> ']'
         | '{' (<expr> ',')* <expr> '}'
         | '{' (<expr> ':' <expr> ',')* <expr> ':' <expr> '}'

    atom = ID
         | NUMBER

# IRs

    Const = Int isize
          | Float fsize
          | Char char
          | String String
          | Symbol Symbol

## Concrete Syntax Tree

    CST = Block  { pos: SrcPos, stmts: CST* }
        | Def    { pos: SrcPos, pat: CST, val: CST }
        | Params { pos: SrcPos, pat: CST, val: CST }
        | Infix  { pos: SrcPos, op: CST, left: CST, right: CST }
        | App    { pos: SrcPos, op: CST args: CST+ }
        | Tuple  { pos: SrcPos, vals: CST* }
        | Array  { pos: SrcPos, vals: CST* }
        | Set    { pos: SrcPos, vals: CST* }
        | Map    { pos: SrcPos, vals: (CST, CST)* }
        | Id     { pos: SrcPos, name: Symbol }
        | Atom   { pos: SrcPos, val: Const }

# Value Representation

* Values are either *immediate* or *allocated*
* Immediate values include
    - Small integers `v & 3 == 0`
    - Object headers `v & 3 == 3`
    - Small floats `v & 3 == 2`
* Allocated values are represented by pointers tagged so that `v & 3 == 1` and
  include
    - Tagpairs `[Header, ValueRef, ValueRef]`
    - Tuples `[Header, ValueRef*]`
    - Blobs `[Header, u8*]`
