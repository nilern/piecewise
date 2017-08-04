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
                let val (cst, lexer) = invoke lexer
                    val (nextToken, lexer) = PcwsParser.Stream.get lexer
                    val _ = print (PPrint.pretty 80 (Cst.toDoc cst) ^ "\n---\n\n")
                    val ast = DesugarBinds.desugar cst
                    val _ = print (PPrint.pretty 80 (Ast.toDoc ast) ^ "\n---\n\n")
                    val aast = DesugarAugs.desugar ast
                    val _ = print (PPrint.pretty 80 (AuglessAst.toDoc aast) ^ "\n---\n\n")
                    val fast0 = ConvertLEnv.convert aast
                    val _ = print (PPrint.pretty 80 (FlatAst0.toDoc fast0) ^ "\n---\n\n")
                    val fast1 = ConvertDEnv.convert fast0
                    val _ = print (PPrint.pretty 80 (FlatAst1.toDoc fast1) ^ "\n---\n\n")
                    val anf = AnfConvert.convert fast1
                    val _ = print (PPrint.pretty 80 (Anf.toDoc anf) ^ "\n---\n\n")
                    val cps = CpsConvert.convert anf
                    val _ = print (PPrint.pretty 80 (Cps.toDoc cps))
                in
                    if PcwsParser.sameToken(nextToken, dummyEOF)
                    then ()
                    else loop lexer
                end
        in
            loop lexer
            handle
                ConvertLEnv.Unbound (pos, name) =>
                    print ("Unbound name: " ^ Name.toString name ^
                           " at " ^ Pos.toString pos ^ "\n")
              | DesugarBinds.Pattern (pos, pat) =>
                    print ("Invalid pattern: " ^
                           PPrint.pretty 80 (Cst.exprToDoc pat) ^
                           " at " ^ Pos.toString pos ^ "\n")
              | DesugarAugs.ReAssignment (pos, var) =>
                    print ("Reassignment of " ^
                           PPrint.pretty 80 (AVar.toDoc var) ^
                           " at " ^ Pos.toString pos ^ "\n")
        end
end (* structure Parser *)

val _ = Parser.parse ()
