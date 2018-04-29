functor PcwsLexerFun(structure Tokens: Pcws_TOKENS): sig
    include ARG_LEXER

    exception LexerError of Pos.t
end = struct
    structure UserDeclarations = struct
        type ('a, 'b) token = ('a, 'b) Tokens.token
        type pos = Pos.t
        type svalue = Tokens.svalue
        type arg = string
    end

    exception LexerError of Pos.t

    val chunkSize = 1024

    fun stream readN =
        let val chars = ref ""
            val index = ref 0

            fun ensureBuf maxIndex =
                if maxIndex < String.size (!chars)
                then true
                else ( chars := (!chars) ^ (readN chunkSize)
                     ; maxIndex < String.size (!chars) )

            fun peek () =
                if ensureBuf (!index)
                then SOME (String.sub (!chars, !index))
                else NONE

            fun pop () =
                if ensureBuf (!index)
                then let val c = String.sub (!chars, !index)
                     in index := !index + 1
                      ; SOME c
                     end
                else NONE

            fun substring (start, size) =
                ( ensureBuf (start + size)
                ; String.substring (!chars, start, size) )
        in {peek, pop, substring}
        end

    fun posStream filename {peek, substring, pop = innerPop} =
        let val pos = ref (Pos.start filename)

            fun pop () =
                case innerPop ()
                of SOME c => (pos := Pos.next (!pos) c; SOME c)
                 | NONE   => NONE
        in {peek, pop, substring, pos = fn () => !pos }
        end

    val isTerminator =
        fn #"(" => true | #")" => true
         | #"[" => true | #"]" => true
         | #"{" => true | #"}" => true
         | #";" => true | #"," => true
         | #"\"" => true | #"'" => true | #"`" => true
         | c => Char.isSpace c

    val isOpChar = Char.contains "!%&*+-./:<=>?\\^|~"

    fun popIf pred {peek, pop, substring = _, pos = _} =
        case peek ()
        of res as (SOME c) => if pred c
                              then (pop (); res)
                              else NONE
         | res as NONE => res

    fun takeWhile pred (input as {peek, pop, substring, pos}) =
        let val startIndex = #index (pos ())

            fun loop oc = Option.app (fn _ => loop (popIf pred input)) oc
            val _ = loop (popIf pred input)

            val endIndex = #index (pos())
        in substring (startIndex, endIndex - startIndex)
        end

    fun takeUntil pred = takeWhile (not o pred)

    fun makeLexer readN filename =
        let val input as {peek, pop, pos, ...} = posStream filename (stream readN)
            fun lexer () =
                let val startPos = pos ()
                in  case peek ()
                    of SOME #"#" => (takeUntil (fn c => c = #"\n") input; lexer ())

                     | SOME #"(" => (pop (); Tokens.LPAREN (startPos, pos ()))
                     | SOME #")" => (pop (); Tokens.RPAREN (startPos, pos ()))
                     | SOME #"[" => (pop (); Tokens.LBRACKET (startPos, pos ()))
                     | SOME #"]" => (pop (); Tokens.RBRACKET (startPos, pos ()))
                     | SOME #"{" => (pop (); Tokens.LBRACE (startPos, pos ()))
                     | SOME #"}" => (pop (); Tokens.RBRACE (startPos, pos ()))

                     | SOME #"," => (pop (); Tokens.COMMA (startPos, pos ()))
                     | SOME #";" => (pop (); Tokens.SEMI (startPos, pos ()))

                     | SOME #"\"" => let val _ = pop ()
                                         val cs = takeUntil (fn c => c = #"\"") input
                                         val _ = pop ()
                                     in Tokens.STRING (cs, startPos, pos ())
                                     end
                     | SOME #"'" => let val _ = pop ()
                                        val cs = takeUntil (fn c => c = #"'") input
                                        val _ = pop ()
                                     in Tokens.CHAR (cs, startPos, pos ())
                                     end

                     | SOME #"$" => let val _ = pop ()
                                        val cs = takeUntil isTerminator input
                                    in Tokens.DYNID (cs, startPos, pos ())
                                    end
                     | SOME #"_" => ( pop ()
                                    ; case peek ()
                                      of SOME c =>
                                          (case c
                                           of #"_" => let val _ = pop ()
                                                          val cs = takeUntil isTerminator input
                                                      in Tokens.PRIM (cs, startPos, pos ())
                                                      end
                                            | _ => let val cs = takeUntil isTerminator input
                                                   in Tokens.DUMID (cs, startPos , pos ())
                                                   end)
                                       | NONE => raise Fail ("unimplemented: _<EOF>") )

                     | SOME c =>
                        if Char.isSpace c
                        then (pop (); lexer ())
                        else if Char.isDigit c
                        then let val ics = takeWhile Char.isDigit input
                             in Tokens.INT (ics, startPos, pos ())
                             end
                        else if Char.isAlpha c
                        then let val cs = takeUntil isTerminator input
                             in Tokens.LEXID (cs, startPos, pos ())
                             end
                        else if isOpChar c
                        then let val cs = takeUntil isTerminator input
                             in  case cs
                                 of "="  => Tokens.EQ (startPos, pos ())
                                  | "|"  => Tokens.BAR (startPos, pos ())
                                  | "->" => Tokens.RARROW (startPos, pos ())
                                  | "<-" => Tokens.LARROW (startPos, pos ())
                                  | "=>" => Tokens.DARROW (startPos, pos ())
                                  | _    => Tokens.OP0 (cs, startPos, pos ())
                             end
                        else raise Fail ("unimplemented " ^ (String.str c))

                     | NONE => Tokens.EOF (startPos, startPos)
                end
        in lexer
        end
end
