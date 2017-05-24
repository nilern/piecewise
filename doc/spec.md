# Lexical Syntax

TODO: peculiar identifiers (_, $..., @...), comments (with #)

QUESTION: what about ``?

    @DELIMITER = ['"`()[]{}]
    @SEPARATOR = [,;]
    @TERMINATOR = \s | @DELIMITER | @SEPARATOR
    @CONSTITUENT = [^@TERMINATOR]

    NUMBER = [\d] @CONSTITUENT*
    ID = [\p{Alphabetic}_@] @CONSTITUENT*
    OP<precedence> = [^\d\p{Alphabetic}_@@TERMINATOR] @CONSTITUENT*
    CHAR = ' [^']+ '
    STRING = " [^"]* "

# Context-Free Syntax

## Top Level

    program = semicolonList<stmt>

## Statements and Cases

    stmt = many1<pattern> '=' stmt
         | many1<pattern> '+=' stmt
         | expr

    case = many1<pattern> '=>' semicolonList<stmt>

## Expressions

    expr = infix<1>

    infix<7> = app
             | infix<7> OP<7> app

    infix<n> = infix<n + 1>
             | infix<n> OP<n> infix<n + 1>

    app = many1<simple>

    simple = '(' expr ')'
           | '{' semicolonList<stmt> '}'
           | '{' semicolonList<case> '}'
           | '[' semicolonList<stmt> ']'
           | ID
           | datum

## Data

    datum = prim
          | collection

    prim = NUMBER
         | STRING
         | CHAR

    collection = compound<expr>

## Patterns

    pattern = ID
            | prim
            | collectionPattern

    collectionPattern = compound<pattern>

## Template Utils

    compound<p> = '(' commaList<p> ')'
                | '[' commaList<p> ']'
                | '{' commaList<p> '}'
                | '{' pairList<p> '}'

    many1<p> = p
             | many1<p> p

    pairList<p> = '->'
                | p '->' p
                | pairList<p> ',' p '->' p

    semicolonList<p> = p
                     | semicolonList<p> ';' p

    commaList<p> = empty
                 | p ','
                 | commaListTwoPlus<p>

    commaListTwoPlus<p> = p ',' p
                        | commaListTwoPlus<p> ',' p

# PEG

## Top Level

    program = semiColonList(stmt)

## Statements and Cases

    stmt = formals '=' stmt
         / formals '+=' stmt
         / expr

    case = formals '=>' semiColonList(stmt)

## Expressions

    expr = infix(1)

    infix(7) = app (op(7) app)*
    infix(n) = infix(n - 1) (op(n) infix(n - 1))*

    app = simple+

    simple = '(' expr ')'
           / '{' semicolonList(case) '}'
           / '{' semicolonList(stmt) '}'
           / '[' stmt (';' stmt)* ']'
           / ID
           / datum

## Data

    datum = prim
          / collection

    prim = NUMBER
         / STRING
         / CHAR

    collection = compound(expr)

## Patterns

    formals = pattern+

    pattern = ID
            / prim
            / compound(pattern)

## Higher Order Utils

    compound(p) = '(' commaList(p) ')'
                / '[' commaList(p) ']'
                / '{' commaList(p) '}'
                / '{' mapList(p) '}'

    semicolonList(p) = p (';' p)*

    commaList(p) = p (',' p)+
                 / p ','
                 / epsilon

    mapList(p) = p '->' p (',' p '->' p)*
               / '->'

# IRs

    data Const = Int IntInf
               | Float FloatInf
               | Char Char
               | String UTFString
               | Symbol UTFString

    data Var = LexVar UTFString
             | DynVar UTFString

    data AVar = GlobVar Name
              | LexVar Name
              | DynVar Name

    data Name = Name UTFString
              | UniqueName UTFString Int

    module CST where
        data Expr = Fn (Expr+, Expr, Expr)+
                  | Block Stmt+
                  | App Expr Expr+
                  | PrimApp PrimOp Expr+
                  | Var Var
                  | Const Const

        data Stmt = Def Expr Expr
                  | AugDef Expr Expr
                  | Expr Expr

    module AST var where
        data Expr = Fn [(var+, Expr, Expr)]
                  | Block Stmt+
                  | If Expr Expr Expr
                  | App Expr Expr+
                  | PrimApp PrimOp Expr+
                  | Var var
                  | Const Const

        data Stmt = Def var Expr
                  | Expr Expr

# Compilation Pipeline

    Text
    -(Lexer)-> Tokens -(WSLexer)-> Tokens
    -(parse)-> CST
    -(desugarScope)-> AST

desugarScope is an alphatization pass that embeds pattern expansion and
AugDef hoisting.

## Lexer

Transform text into token stream.

## WSLexer

Add whitespace-based tokens (NEWLINE, INDENT, DEDENT) into token stream.

## Parse

Parse token stream into AST.

## Expand

Expand macros. A preorder traversal that iterates on every node until
stabilization.

## Flatten

Identify variable references as Local, Clover or Global, alphatize and perform
closure conversion, yielding an alphatized FAST (First order AST). A 'down-up'
traversal that uniquely renames variables on the way down with the help of an
inherited environment attribute and records closed over variables on lambdas and
creates the FAST on the way up.

## CPSConvert

Perform CPS conversion. Some sort of post order traversal, needs to be
clarified. At least it is not the extremely confusing higher-order one.

## Liveness

Annotate escaping closures with the variables they need to close over (which
they do by saving them on the stack below the frame pointer and return address
stuff).

## RegAlloc

Register allocation. With the current instruction design it is awkward to use
more than 64 registers. More importantly each closure creation and call requires
shuffling the registers carefully while minimizing the amount of moves as much
as possible.

## bytecode::Assembler

Feed the low-level CPS program to the bytecode assembler to obtain bytecode
objects. The assembler takes care of the tedious details of clover, const and
global indexing as well as jump offset resolution.

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

# VM

## ISA

### Arithmetic

#### Binops

\((a: Int_{l|c}, b: Int_{l|c}) \rightarrow \kappa(d: Int)\)

    iadd d a b
    isub d a b
    imul d a b
    idiv d a b

\((a: Int_{l|c}, b: Int_{l|c}) \rightarrow \kappa(d: Bool)\)

    ieq d a b
    ile d a b
    ige d a b
    ilt d a b
    igt d a b

### Branches

\(() \rightarrow \kappa()\)

    br offset

\((cond: Bool_{l|c}) \rightarrow \kappa_1() | \kappa_2()\)

    brf cond offset

#### Calls

\(() \rightarrow \kappa_1()\)

    call fnreg argc

\(() \rightarrow \kappa_1()\)

    next argc

\((cond: Bool_{l|c}) \rightarrow \kappa_1() | \kappa_2()\)

    nextf cond argc

#### Returns

\((v: Any_{l|c}) \rightarrow \kappa (Any)\)

    ret v
    halt v
