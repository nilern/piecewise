structure Parser :> sig
    val parse : unit -> unit
end = struct
    structure PcwsLrVals =
        PcwsLrValsFun(structure Token = LrParser.Token)

    structure PcwsLex =
        PcwsLexerFun(structure Tokens = PcwsLrVals.Tokens)

    structure PcwsParser =
        JoinWithArg(structure LrParser = LrParser
                    structure ParserData = PcwsLrVals.ParserData
                    structure Lex = PcwsLex)

    val op<+> = PPrint.<+>

    fun invoke lexstream =
        let fun print_error (s, p, _) =
                TextIO.output(TextIO.stdOut, String.concat[
                    "SyntaxError: ", s, " in ", (Pos.toString p), "\n"])
        in
            PcwsParser.parse(0, lexstream, print_error, ())
        end

    fun parse () =
        let val fileName = "#<stdin>"
            val lexer = PcwsParser.makeLexer (fn _ =>
                            (case TextIO.inputLine TextIO.stdIn
                             of SOME s => s
                              | _ => "")) fileName
            (* val _ = PcwsLex.UserDeclarations.initialize () *)
            val startPos = Pos.start fileName
            val dummyEOF = PcwsLrVals.Tokens.EOF(startPos, startPos)
            fun loop lexer =
                let val (ast, lexer) = invoke lexer
                    val (nextToken, lexer) = PcwsParser.Stream.get lexer
                    val _ = print (PPrint.pretty 80 (Value.exprToDoc ast))
                    val evalue = Interpreter.interpret ast
                    val _ = print "==>\n"
                    val _ = print (PPrint.pretty 80 (Value.valueToDoc evalue))
                in  if PcwsParser.sameToken(nextToken, dummyEOF)
                    then ()
                    else loop lexer
                end
        in
            loop lexer
        end
        handle
            PcwsLex.LexerError pos => print ("Lexer Error at " ^ (Pos.toString pos) ^ "\n")
          | Interpreter.Panic exn =>
                print ("Panic: " ^ (PPrint.pretty 80 (Value.valueToDoc exn)))
end

val _ = Parser.parse ()
