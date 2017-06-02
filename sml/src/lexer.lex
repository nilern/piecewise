structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val pos = ref 0

fun eof _ = Tokens.EOF(!pos, !pos)

fun error (e, l : int, _) = TextIO.output (TextIO.stdOut, String.concat [
	    "line ", (Int.toString l), ": ", e, "\n"])

%%

%header (functor PcwsLexFun(structure Tokens: Pcws_TOKENS));

alpha = [A-Za-z];
ws = [\ \t];

%%

\n       => (pos := !pos + 1; continue());
{ws}+    => (continue());
{alpha}+ => (Tokens.ID(yytext, !pos, !pos));
