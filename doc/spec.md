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

# Data

* Injection: creates a new value of type T
* Ejection: disassembles value if it is of type T
* Classification: `typeOf value` returns the type T of value
* Membership: `value : T` tells whether value is a member of T



## Builtin Types

Int, Float, Char, Bool

Symbol

Tuple

Some and None

Fn, Type

Any (?, supporting this would probably mean that
        (:) != { a b => typeOf a == typeOf b})

## User-defined Types

A way to create injector-ejector Fn:s is all that is required, for example:

    (Pair, newPair, viewPair) = recordType :Pair (Any, Any);

    {
        p = newPair 1 2;
        viewPair a b = p;
        p : Pair
    };

For added convenience you can

    apply += {(_ == Pair, fields) => apply newPair fields};
    unapply += {(_ == Pair, v) => viewPair v};

    {
        p = Pair 1 2;
        Pair a b = p;
        p : Pair
    };

Types are first-class objects but not very interesting ones. By default they
only support `toString` and `==`. Type introspection is left unspecified;
implementations are free to provide whatever is convenient (e.g. Types can be
represented as Class objects in a JVM-hosted implementation).

<!-- Could also have some sugar for integer or name-indexed fields (with getters
also returning Option to avoid the Haskell record problems). -->

### Derivation

    typeOf = { v => __typeOf v};

    (:) = { a b => typeOf a == typeOf b};

    Type = __new :TagPair Type :Type;

    toString += {(t : Type) => __repr t};

    recordType = { name fieldTypes =>
        T = __new :TagPair Type name;

        injectT = {
            @args fields | count fields == count fieldTypes
                           @&& every {:} fields fieldTypes =>
                __new :TagPair T fields
        };

        ejectT = {
            value | value : T => Some {__repr value};
            _ => None
        };

        (T, injectT, ejectT)
    };

# Functions

Functions can be

* Applied: `foo bar baz` or `apply foo (bar, baz)`
* Reverse applied: `foo bar baz = foo 1 2` or `unapply foo (foo 1 2)`
* Queried about their domain and range
* Combined: `foo += {3 => 4}` or `foo <|> {3 => 4}`
