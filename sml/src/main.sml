structure Parser : sig
    val parse : unit -> unit
end = struct
    structure PcwsLrVals =
        PcwsLrValsFun(structure Token = LrParser.Token)

    structure PcwsLex =
        PcwsLexFun(structure Tokens = PcwsLrVals.Tokens)

    structure PcwsParser =
        Join(structure LrParser = LrParser
             structure ParserData = PcwsLrVals.ParserData
             structure Lex = PcwsLex)

    fun invoke lexstream =
        let fun print_error (s, i:int, _) =
                TextIO.output(TextIO.stdOut,
                    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
        in
            PcwsParser.parse(0, lexstream, print_error, ())
        end

    fun parse () =
        let val lexer = PcwsParser.makeLexer (fn _ =>
                            (case TextIO.inputLine TextIO.stdIn
                             of SOME s => s
                              | _ => ""))
            val dummyEOF = PcwsLrVals.Tokens.EOF(0, 0)
            fun loop lexer =
                let val (result, lexer) = invoke lexer
                    val (nextToken, lexer) = PcwsParser.Stream.get lexer
                in
                    TextIO.output(TextIO.stdOut, CST.toString result ^ "\n");
                    if PcwsParser.sameToken(nextToken, dummyEOF)
                    then ()
                    else loop lexer
                end
        in
            loop lexer
        end
end (* structure Parser *)

val _ = Parser.parse ()
