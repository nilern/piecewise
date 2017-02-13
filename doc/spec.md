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

# Concrete Syntax Tree

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
        | Atom   { pos: SrcPos, val: Any }

# Abstract Syntax Tree

    AST = Block { pos: SrcPos, decls: String*, stmts: Stmt* }
        | Fn    { pos: SrcPos, clauses: Clause* }
        | App   { pos: SrcPos, op: AST, args: AST+ }
        | Var   { pos: SrcPos, name: Symbol }
        | Const { pos: SrcPos, val: Value }

    Stmt = Def  { pos: SrcPos, pat: AST, val: AST }
         | Expr { pos: SrcPos, expr: AST }

    Clause = Clause {
        pos: SrcPos,
        params: AST+,
        condition: AST,
        body: AST
    }

# Value Representation

* Values are either *immediate* or *allocated*
* Immediate values include
    - Small integers `v & 3 == 0`
    - Object headers `v & 3 == 3`
    - ??? `v & 3 == 2`
* Allocated values are represented by pointers tagged so that `v & 3 == 1` and
  include
    - Tagpairs `[Header, ValueRef, ValueRef]`
    - Tuples `[Header, UInt, ValueRef*]`
    - Blobs `[Header, UInt, u8*]`
