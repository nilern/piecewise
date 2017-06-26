structure Parser : sig
    val parse : unit -> unit
end = struct
    structure PcwsLrVals =
        PcwsLrValsFun(structure Token = LrParser.Token)

    structure PcwsLex =
        PcwsLexFun(structure Tokens = PcwsLrVals.Tokens)

    structure PcwsParser =
        JoinWithArg(structure LrParser = LrParser
                    structure ParserData = PcwsLrVals.ParserData
                    structure Lex = PcwsLex)

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
            val _ = PcwsLex.UserDeclarations.initialize ()
            val startPos = Pos.start fileName
            val dummyEOF = PcwsLrVals.Tokens.EOF(startPos, startPos)
            fun loop lexer =
                let val (result, lexer) = invoke lexer
                    val (nextToken, lexer) = PcwsParser.Stream.get lexer
                in
                    Vector.app
                        (fn cst => TextIO.output(TextIO.stdOut,
                                                 PPrint.pretty 80
                                                     (CST0.stmtToDoc cst)))
                        result;
                    TextIO.output(TextIO.stdOut, "\n\n");
                    let val fcst = LexFlatten.flatten result
                    in TextIO.output(TextIO.stdOut,
                                     PPrint.pretty 80 (FlatCST.toDoc fcst))
                    end;
                    if PcwsParser.sameToken(nextToken, dummyEOF)
                    then ()
                    else loop lexer
                end
        in
            loop lexer
            handle LexFlatten.Unbound (pos, name) =>
                       print ("Unbound name: " ^ StringName.toString name ^
                              " at " ^ Pos.toString pos ^ "\n")
        end
end (* structure Parser *)

val _ = Parser.parse ()
