# Layout and Tagging

At a conceptual (but low) level each Value consists of a bag of bits/bytes and
a TypeRef. The TypeRef is used by the program and runtime to interpret the bag
of bits/bytes.

Since we need to encode recursive types (linked lists and such) and fit values
in registers we actually pass around ValueRef:s instead of the full values.
ValueRef is a "tagged pointer.

## TypeIndices

A TypeIndex is an index into the global type array.

In the general case data is heap-allocated and laid out like this:

    struct {
        header: usize,
        type: usize,
        <fields for data bits>
    }

However if the type index is small enough we can fold it into the ValueRef and
the header word:

    struct {
        header: usize,
        <fields for data bits>
    }

If the payload is also small we can fold it into the ValueRef as well.

The low three bits of ValueRef:s are used to store type indices and GC info:

    0b000: Int   (61-bit signed integer)
    0b100: Float (61-bit signed floating point)
    0b010: Char  (Unicode scalar value)
    0b110: Bool  (true or false) [enough bits for (), None, etc. in here...]
    0b001: Nil   (Uninitialized, NOT the same as None)
    0b101: HeapValue with no ValueRef fields
    0b011: HeapValue with just ValueRef fields
    0b111: HeapValue with a mix of ValueRef and bitbag fields

Inspection of the individual low bits of a ValueRef reveals the following:

    match v & 1 {
        0 => {
            // v is immediate
            match v & 0b10 {
                0 => { /* v is numeric */ },
                1 => { /* v is not numeric */ }
            }
        },
        1 => {
            // v contains a pointer
            match v & 0b110 {
                0b000 => { /* v is Nil, GC must not travers */ },
                0b100 => {
                    /* v contains no ValueRef fields, GC need not traverse */
                },
                0b010 => { /* v contains just ValueRef fields */ },
                0b110 => { /* v contains a mix of ValueRef and bitbag fields */ }
            }
        }
    }


## Field Layout

Every HeapValue starts with a header field.

<!-- Header needs to store type tag, granule size, ValueRef fields (offset and
     count) -->

The header tag carries the following information:

    0b000:
    0b100:
    0b010:
    0b110:
    0b001:
    0b101: Variant with no ValueRef fields
    0b011: Variant with just ValueRef fields
    0b111: Variant with a mix of ValueRef and bitbag fields

<!-- Blob, Tuple, Indirect, Closure, Fn, TagPair -->

The fields are stored sorted in order of decreasing alignment requirement.
Pointer fields are sorted before other word-sized fields.

The garbage collector needs to be able to iterate over the potential pointer
fields of the Values. Thanks to the field layout it can do this based solely on:

    1. The byte offset of the first pointer field (often 0)
    2. The number of pointer fields (often found directly in header)

The GC can find the pointers as follows:

    1. If the last bits of the ValueRef are not 11, there are none.
    2. If the ValueRef tag is 0b011, there are (*v).header.length() ValueRefs at
    3. If the ValueRef tag is 0b011,

## Object Headers

# Primops

## Typehecks and -casts

    fn isa[T](v: Any) -> Proof[ isa[T](v) ] throws Type { assert[Type](v & 0b111 == T::TAG) }

    fn isPtr(v: Any) -> Proof[ isPtr(v) ] throws Type { assert[Type](v & 1 == 1) }

    fn downcast(p: Proof[ isa[T](v) ])(v: Any) -> T { v }

    fn unbox[T <: Int|Char|Bool](p: Proof[ isa[T](v) ])(v: T) -> isize { v >> 3 }
    fn unbox[T <: Float](p: Proof[ isa[Float](v) ])(v: Float) -> fsize { v & !0b111 }

    fn address[T](p: Proof[ isPtr(v) ])(v: Ptr[T]) -> address[T] { v & !0b111 }

## Arithmetic

    fn iadd(a: Int, b: Int) -> Int throws Overflow
    fn iadds(a: Int, b: Int, onOverflow: (Int) -> Any) -> Any

    fn isub(a: Int, b: Int) -> Int throws Overflow
    fn isubs(a: Int, b: Int, onOverflow: (Int) -> Any) -> Any

    fn imul(a: Int, b: Int) -> Int throws Overflow
    fn imuls(a: Int, b: Int, onOverflow: (Int, Int) -> Any) -> Any

    fn idiv(a: Int, b: Int) -> Int throws Overflow | DivByZero

    fn idivRem(a: Int, b: Int, onOverflow: (Int, Int) -> Any) -> Any throws Overflow | DivByZero

### CPS

    fn iadd(a: Int, b: Int)(ok: cont(Int), err: cont(Overflow))
    fn iadds(a: Int, b: Int)(ok: cont(Int), err: cont(Int, Int))

## Allocation

    fn new[T <: Allocatable](vs: Any,*) -> T throws OOM

## Tagpairs

    fn tag(v: TagPair) -> Any { v[0] }

    fn repr(v: TagPair) -> Any { v[1] }

## Indirections

    fn redirect[T](v: Indirection[T]) -> T throws Uninitialized

# IRs

    data Stmt = Def Pos Pattern (Option Expr) Expr
              | Expr Expr

    data Expr = Fn Pos (Option Name) [([Pattern], Option Expr, Expr)]
              | Block Pos [Stmt] Expr
              | Call Pos Expr [Expr]
              | PrimCall Pos Primop [Expr]
              | Triv Pos (Triv RVar)

    data Pattern = Call Pos Expr [Pattern]
                 | Triv Pos (Triv PatVar)

    data Triv rvar = Const Const
                   | Var rvar

    data BaseVar = Lex Name
                 | Dyn Name

    data RVar = Current BaseVar
              | Upper BaseVar

    data PatVar = Bind BaseVar
                | Ignore

    data Const = Int IntInf
               | Float FloatInf
               | Char Char
               | Bool Bool
               | String UTFString
               | Symbol UTFString

<!--

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
    - Blobs `[Header, u8*]` -->
