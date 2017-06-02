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
end

fun eof fileName = let val p = getPos fileName in Tokens.EOF(p, p) end

fun error (e, p, _) = TextIO.output(TextIO.stdOut, Pos.toString p ^ "\n")

%%

%header (functor PcwsLexFun(structure Tokens: Pcws_TOKENS));
%arg (fileName : string);

delimiter = ['\"`\(\)\[\]\{\}];
separator = [,\;];

alpha = [A-Za-z];
ws = [\ \t];

%%

{ws}+    => (incCol (size yytext); continue());
# [^\n]* => (incCol (size yytext); continue());
\n       => (incLine (); continue());

{alpha}+ => (Tokens.ID (advance yytext fileName));

"="      => (Tokens.EQ (advance_ yytext fileName));
"+="     => (Tokens.AUG (advance_ yytext fileName));
"=>"     => (Tokens.DARROW (advance_ yytext fileName));
"->"     => (Tokens.ARROW (advance_ yytext fileName));

\(       => (Tokens.LPAREN (advance_ yytext fileName));
\)       => (Tokens.RPAREN (advance_ yytext fileName));
\[       => (Tokens.LBRACKET (advance_ yytext fileName));
\]       => (Tokens.RBRACKET (advance_ yytext fileName));
\{       => (Tokens.LBRACE (advance_ yytext fileName));
\}       => (Tokens.RBRACE (advance_ yytext fileName));

,        => (Tokens.COMMA (advance_ yytext fileName));
\;       => (Tokens.SEMI (advance_ yytext fileName));
