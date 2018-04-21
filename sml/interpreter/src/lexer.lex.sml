(*#line 65.10 "lexer.lex"*)functor PcwsLexFun(structure Tokens: Pcws_TOKENS)(*#line 1.1 "lexer.lex.sml"*)
=
   struct
    structure UserDeclarations =
      struct
(*#line 1.1 "lexer.lex"*)(* FIXME: [] in constituent *)

structure Tokens = Tokens

type pos = Pos.t
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token
type arg = string

local
    val pos = ref (0, 1, 1)
in
    fun initialize () = pos := (0, 1, 1)
    fun getPos fileName =
            let val (index, line, col) = !pos
            in {file = fileName, index = index, line = line, col = col} end
    fun incCol n =
            let val (index, line, col) = !pos
            in pos := (index + n, line, col + n) end
    fun incLine () =
            let val (index, line, col) = !pos
            in pos := (index + 1, line + 1, 1) end
    fun advance cs fileName =
            let val p = getPos fileName
                val _ = incCol (size cs)
                val q = getPos fileName
            in (cs, p, q) end
    fun advance_ cs fileName =
            let val p = getPos fileName
                val _ = incCol (size cs)
                val q = getPos fileName
            in (p, q) end
    fun advanceMap f cs fileName =
            let val p = getPos fileName
                val _ = incCol (size cs)
                val q = getPos fileName
            in (f cs, p, q) end
    fun advanceOp cs fileName =
            let val p = getPos fileName
                val _ = incCol (size cs)
                val q = getPos fileName
                val make = case Name.precOf cs
                           of Name.Zero  => Tokens.OP0
                            | Name.One   => Tokens.OP1
                            | Name.Two   => Tokens.OP2
                            | Name.Three => Tokens.OP3
                            | Name.Four  => Tokens.OP4
                            | Name.Five  => Tokens.OP5
                            | Name.Six   => Tokens.OP6
                            | Name.Seven => Tokens.OP7
            in make (cs, p, q) end
end

fun stripQuotes s = substring(s, 1, String.size s - 2)
fun drop n s = String.extract(s, n, NONE)
fun withPrec cs = (cs, Name.precOf cs)

fun eof fileName = let val p = getPos fileName in Tokens.EOF(p, p) end

fun error (e, p, _) = TextIO.output(TextIO.stdOut, Pos.toString p ^ "\n")

(*#line 68.1 "lexer.lex.sml"*)
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\000\000\000\000\000\000\000\000\000\028\029\000\000\028\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\028\003\026\025\024\003\003\022\021\020\003\018\017\015\003\003\
\\014\014\014\014\014\014\014\014\014\014\000\013\003\011\003\003\
\\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\010\003\009\003\007\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\005\003\004\003\000\
\\000"
),
 (3, 
"\003\003\003\003\003\003\003\003\003\000\000\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\000\003\000\003\003\003\003\000\000\000\003\003\000\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\000\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\000\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\000\003\000\003\003\
\\003"
),
 (6, 
"\006\006\006\006\006\006\006\006\006\000\000\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\000\006\000\006\006\006\006\000\000\000\006\006\000\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\006\000\006\006\
\\006"
),
 (7, 
"\006\006\006\006\006\006\006\006\006\000\000\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\000\006\000\006\006\006\006\000\000\000\006\006\000\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\008\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\006\000\006\006\
\\006"
),
 (8, 
"\008\008\008\008\008\008\008\008\008\000\000\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\000\008\000\008\008\008\008\000\000\000\008\008\000\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\008\000\008\008\
\\008"
),
 (11, 
"\003\003\003\003\003\003\003\003\003\000\000\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\000\003\000\003\003\003\003\000\000\000\003\003\000\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\000\003\003\012\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\000\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\000\003\000\003\003\
\\003"
),
 (14, 
"\014\014\014\014\014\014\014\014\014\000\000\014\014\014\014\014\
\\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014\
\\000\014\000\014\014\014\014\000\000\000\014\014\000\014\014\014\
\\014\014\014\014\014\014\014\014\014\014\014\000\014\014\014\014\
\\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014\
\\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014\
\\000\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014\
\\014\014\014\014\014\014\014\014\014\014\014\000\014\000\014\014\
\\014"
),
 (15, 
"\003\003\003\003\003\003\003\003\003\000\000\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\000\003\000\003\003\003\003\000\000\000\003\003\000\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\000\003\003\016\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\000\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\000\003\000\003\003\
\\003"
),
 (18, 
"\003\003\003\003\003\003\003\003\003\000\000\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\000\003\000\003\003\003\003\000\000\000\003\003\000\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\000\003\019\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\000\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\000\003\000\003\003\
\\003"
),
 (22, 
"\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\
\\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\
\\022\022\000\022\022\022\022\023\022\022\022\022\022\022\022\022\
\\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\
\\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\
\\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\
\\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\
\\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\
\\022"
),
 (24, 
"\024\024\024\024\024\024\024\024\024\000\000\024\024\024\024\024\
\\024\024\024\024\024\024\024\024\024\024\024\024\024\024\024\024\
\\000\024\000\024\024\024\024\000\000\000\024\024\000\024\024\024\
\\024\024\024\024\024\024\024\024\024\024\024\000\024\024\024\024\
\\024\024\024\024\024\024\024\024\024\024\024\024\024\024\024\024\
\\024\024\024\024\024\024\024\024\024\024\024\024\024\024\024\024\
\\000\024\024\024\024\024\024\024\024\024\024\024\024\024\024\024\
\\024\024\024\024\024\024\024\024\024\024\024\000\024\000\024\024\
\\024"
),
 (25, 
"\025\025\025\025\025\025\025\025\025\025\000\025\025\025\025\025\
\\025\025\025\025\025\025\025\025\025\025\025\025\025\025\025\025\
\\025\025\025\025\025\025\025\025\025\025\025\025\025\025\025\025\
\\025\025\025\025\025\025\025\025\025\025\025\025\025\025\025\025\
\\025\025\025\025\025\025\025\025\025\025\025\025\025\025\025\025\
\\025\025\025\025\025\025\025\025\025\025\025\025\025\025\025\025\
\\025\025\025\025\025\025\025\025\025\025\025\025\025\025\025\025\
\\025\025\025\025\025\025\025\025\025\025\025\025\025\025\025\025\
\\025"
),
 (26, 
"\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\027\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026"
),
 (28, 
"\000\000\000\000\000\000\000\000\000\028\000\000\000\028\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 58)], trans = 3},
{fin = [(N 30)], trans = 0},
{fin = [(N 28)], trans = 0},
{fin = [(N 55)], trans = 6},
{fin = [(N 55)], trans = 7},
{fin = [(N 49),(N 55)], trans = 8},
{fin = [(N 26)], trans = 0},
{fin = [(N 24)], trans = 0},
{fin = [(N 9),(N 58)], trans = 11},
{fin = [(N 15),(N 58)], trans = 3},
{fin = [(N 34)], trans = 0},
{fin = [(N 37)], trans = 14},
{fin = [(N 58)], trans = 15},
{fin = [(N 18),(N 58)], trans = 3},
{fin = [(N 32)], trans = 0},
{fin = [(N 58)], trans = 18},
{fin = [(N 12),(N 58)], trans = 3},
{fin = [(N 22)], trans = 0},
{fin = [(N 20)], trans = 0},
{fin = [], trans = 22},
{fin = [(N 45)], trans = 22},
{fin = [(N 52),(N 55)], trans = 24},
{fin = [(N 5)], trans = 25},
{fin = [], trans = 26},
{fin = [(N 41)], trans = 0},
{fin = [(N 2)], trans = 28},
{fin = [(N 7)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

structure YYPosInt : INTEGER = Int
fun makeLexer yyinput =
let	val yygone0= YYPosInt.fromInt ~1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex (yyarg as ((*#line 66.7 "lexer.lex"*)fileName : string(*#line 309.1 "lexer.lex.sml"*)
)) =
let fun continue() : Internal.result = 
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = YYPosInt.+(YYPosInt.fromInt i0, !yygone)
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  12 => let val yytext=yymktext() in (*#line 82.14 "lexer.lex"*)Tokens.AUG (advance_ yytext fileName)(*#line 325.1 "lexer.lex.sml"*)
 end
| 15 => let val yytext=yymktext() in (*#line 83.14 "lexer.lex"*)Tokens.DARROW (advance_ yytext fileName)(*#line 327.1 "lexer.lex.sml"*)
 end
| 18 => let val yytext=yymktext() in (*#line 84.14 "lexer.lex"*)Tokens.ARROW (advance_ yytext fileName)(*#line 329.1 "lexer.lex.sml"*)
 end
| 2 => let val yytext=yymktext() in (*#line 77.14 "lexer.lex"*)incCol (size yytext); continue()(*#line 331.1 "lexer.lex.sml"*)
 end
| 20 => let val yytext=yymktext() in (*#line 86.14 "lexer.lex"*)Tokens.LPAREN (advance_ yytext fileName)(*#line 333.1 "lexer.lex.sml"*)
 end
| 22 => let val yytext=yymktext() in (*#line 87.14 "lexer.lex"*)Tokens.RPAREN (advance_ yytext fileName)(*#line 335.1 "lexer.lex.sml"*)
 end
| 24 => let val yytext=yymktext() in (*#line 88.14 "lexer.lex"*)Tokens.LBRACKET (advance_ yytext fileName)(*#line 337.1 "lexer.lex.sml"*)
 end
| 26 => let val yytext=yymktext() in (*#line 89.14 "lexer.lex"*)Tokens.RBRACKET (advance_ yytext fileName)(*#line 339.1 "lexer.lex.sml"*)
 end
| 28 => let val yytext=yymktext() in (*#line 90.14 "lexer.lex"*)Tokens.LBRACE (advance_ yytext fileName)(*#line 341.1 "lexer.lex.sml"*)
 end
| 30 => let val yytext=yymktext() in (*#line 91.14 "lexer.lex"*)Tokens.RBRACE (advance_ yytext fileName)(*#line 343.1 "lexer.lex.sml"*)
 end
| 32 => let val yytext=yymktext() in (*#line 93.14 "lexer.lex"*)Tokens.COMMA (advance_ yytext fileName)(*#line 345.1 "lexer.lex.sml"*)
 end
| 34 => let val yytext=yymktext() in (*#line 94.14 "lexer.lex"*)Tokens.SEMI (advance_ yytext fileName)(*#line 347.1 "lexer.lex.sml"*)
 end
| 37 => let val yytext=yymktext() in (*#line 96.27 "lexer.lex"*)Tokens.INT (advance yytext fileName)(*#line 349.1 "lexer.lex.sml"*)
 end
| 41 => let val yytext=yymktext() in (*#line 98.6 "lexer.lex"*)Tokens.STRING (advanceMap stripQuotes yytext fileName)(*#line 351.1 "lexer.lex.sml"*)
 end
| 45 => let val yytext=yymktext() in (*#line 100.6 "lexer.lex"*)Tokens.CHAR (advanceMap stripQuotes yytext fileName)(*#line 353.1 "lexer.lex.sml"*)
 end
| 49 => let val yytext=yymktext() in (*#line 102.28 "lexer.lex"*)Tokens.PRIM (advanceMap (drop 2) yytext fileName)(*#line 355.1 "lexer.lex.sml"*)
 end
| 5 => let val yytext=yymktext() in (*#line 78.14 "lexer.lex"*)incCol (size yytext); continue()(*#line 357.1 "lexer.lex.sml"*)
 end
| 52 => let val yytext=yymktext() in (*#line 103.28 "lexer.lex"*)Tokens.DYNID (advanceMap (drop 1) yytext fileName)(*#line 359.1 "lexer.lex.sml"*)
 end
| 55 => let val yytext=yymktext() in (*#line 104.28 "lexer.lex"*)Tokens.LEXID (advance yytext fileName)(*#line 361.1 "lexer.lex.sml"*)
 end
| 58 => let val yytext=yymktext() in (*#line 105.28 "lexer.lex"*)advanceOp yytext fileName(*#line 363.1 "lexer.lex.sml"*)
 end
| 7 => ((*#line 79.14 "lexer.lex"*)incLine (); continue()(*#line 365.1 "lexer.lex.sml"*)
)
| 9 => let val yytext=yymktext() in (*#line 81.14 "lexer.lex"*)Tokens.EQ (advance_ yytext fileName)(*#line 367.1 "lexer.lex.sml"*)
 end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof yyarg
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := YYPosInt.+(!yygone, YYPosInt.fromInt i0);
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
in continue end
  in lex
  end
end
