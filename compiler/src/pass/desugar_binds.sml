(* TODO: call this `ApplyUnApply` and perform also the `foo bar baz -> apply foo (bar, baz)` *)

structure DesugarBinds :> sig
    val desugar : (Cst.expr, Cst.stmt) Block.t -> (Ast.expr, Ast.stmt) Block.t

    exception Pattern of Pos.t * Cst.expr
end = struct
    structure CExpr = Cst.Expr
    structure CTriv = CExpr.Triv

    structure Expr = Ast.Expr
    structure Stmt = Ast.Stmt
    structure Triv = Ast.Expr.Triv

    val FixE = Ast.FixE
    val FixS = Ast.FixS
    val Prolog = Ast.Prolog
    val Fn = Expr.Fn
    val Block = Expr.Block
    val Call = Expr.Call
    val PrimCall = Expr.PrimCall
    val Triv = Expr.Triv
    val Def = Stmt.Def
    val Guard = Stmt.Guard
    val Var = Triv.Var
    val Const = Triv.Const

    exception Pattern of Pos.t * Cst.expr

    fun expandPat newBinding pat access parentId (cond, binds) =
        case Cst.unwrapE pat
        of CExpr.Fn _ => raise Pattern (Cst.exprPos pat, pat)
         | CExpr.Block _ => raise Pattern (Cst.exprPos pat, pat)
         | CExpr.Call (pos, f, args) =>
           let val unapply = FixE (Triv (pos, Var (BaseVar.Lex (Name.fromString "unapply"))))
               val eq = FixE (Triv (pos, Var (BaseVar.Lex (Name.fromString "=="))))
               val f' = expandExpr f
               val access' = FixE (Call (pos, unapply, Vector.fromList [f', access]))
               val tag = FixE (Expr.PrimCall (pos, Primop.Tag, VectorExt.singleton access'))
               val some = FixE (Triv (pos, Const (Const.Symbol "Some")))
               val newCondExpr = FixE (Call (pos, eq, Vector.fromList [tag, some]))
               val (newCond, parentId') = DNF.require newCondExpr (OptionExt.toList parentId)
               val cond' = DNF.conj (Vector.fromList [cond, newCond])
               val access'' = FixE (Expr.PrimCall (pos, Primop.Repr, Vector.fromList [access']))
           in expandArgs newBinding args access'' (SOME parentId') (cond', binds)
           end
         | CExpr.PrimCall (_, po, args) => raise Fail "unimplemented"
         | CExpr.Triv (_, CTriv.Var var) =>
           (cond, VectorExt.conj binds (newBinding (Cst.exprPos pat, var, access)))
         | CExpr.Triv (pos, CTriv.Const c) =>
           let val eq = FixE (Triv (pos, Var (BaseVar.Lex (Name.fromString "=="))))
               val c' = FixE (Triv (pos, Const c))
               val newCondExpr = FixE (Call (pos, eq, Vector.fromList [access, c']))
               val (newCond, _) = DNF.require newCondExpr (OptionExt.toList parentId)
           in
               ( DNF.conj (Vector.fromList [cond, newCond])
               , binds )
           end

    and expandArgs newBinding args parentAccess parentId (cond, binds) =
        let fun expandArg (i, arg) =
                let val pos = Cst.exprPos arg
                    val ie = FixE (Triv (pos, Const (Const.Int (Int.toString i))))
                    val argAccess = FixE (Expr.PrimCall (pos, Primop.AGet,
                                                        Vector.fromList [parentAccess, ie]))
                in expandPat newBinding arg argAccess parentId
                             (DNF.always (), VectorExt.empty ())
                end
            val argCbs = Vector.mapi expandArg args
            val argConds = Vector.map #1 argCbs
            val cond' = DNF.conj (VectorExt.prepend argConds cond)
            fun step ((_, bs), acc) = VectorExt.concat acc bs
            val binds' = Vector.foldl step binds argCbs
        in (cond', binds')
        end

    and expandBind newBinding (Cst.Bind (pat, cond)) access =
        let val (cond', bindStmts) =
                expandPat newBinding pat access NONE (DNF.always (), VectorExt.empty ())
        in
            if DNF.isAlways cond'
            then bindStmts
            else VectorExt.prepend bindStmts (FixS (Guard (Cst.exprPos pat, cond')))
        end

    and expandPrologue pos (Cst.Prolog (args, cond)) params =
        let val access = FixE (Triv (pos, Var (BaseVar.Lex params)))
            fun newBinding (pos, var, expr) = FixS (Def (pos, LVar.Def var, expr))
        in Prolog (expandArgs newBinding args access NONE (DNF.always (), VectorExt.empty ()))
        end

    and expandExpr (Cst.FixE expr) =
        FixE (case expr
              of CExpr.Fn (pos, name, params, cases) =>
                 let fun expandCase (prologue, body) =
                         (expandPrologue pos prologue params, expandExpr body)
                 in Fn (pos, name, params, Vector.map expandCase cases)
                 end
               | CExpr.Block (pos, block) => Block (pos, expandBlock block)
               | CExpr.Call (pos, f, args) =>
                 Call (pos, expandExpr f, Vector.map expandExpr args)
               | CExpr.PrimCall (pos, po, args) => PrimCall (pos, po, Vector.map expandExpr args)
               | CExpr.Triv (pos, CTriv.Var v) =>  Triv (pos, Var v)
               | CExpr.Triv (pos, CTriv.Const c) => Triv (pos, Const c))

    and expandStmt (Cst.FixS stmt) =
        let fun expandDef newDef (bind as Cst.Bind (pat, _)) expr =
                (* FIXME: tuple etc. pats get treated as fn definitions *)
                case bind
                of Cst.Bind (Cst.FixE (CExpr.Call (pos, f, args)), cond) =>
                   let val name = case f
                                  of Cst.FixE (CExpr.Triv (_, CTriv.Var name)) => SOME name
                                   | _ => NONE
                       val params = Name.freshFromString "params"
                       val cases = VectorExt.singleton (Cst.Prolog (args, cond), expr)
                   in expandDef newDef (Cst.Bind (f, NONE))
                                       (Cst.FixE (CExpr.Fn (pos, name, params, cases)))
                   end
                 | _ =>
                   (case expandExpr expr
                    of expr' as Ast.FixE (Expr.Triv _) => expandBind (FixS o newDef) bind expr'
                     | expr' => let val pos = Cst.exprPos pat
                                    val triv = Name.freshFromString "v"
                                    val trivDef =
                                        FixS (Def (pos, LVar.Def (BaseVar.Lex triv),
                                                   expr'))
                                    val trivUse = FixE (Expr.Triv (pos, Var (BaseVar.Lex triv)))
                                    val bindStmts = expandBind (FixS o newDef) bind trivUse
                                in VectorExt.prepend bindStmts trivDef
                                end)
        in
            case stmt
            of Cst.Stmt.Def (bind, expr) =>
               expandDef (fn (pos, var, expr) => Def (pos, LVar.Def var, expr))
                         bind expr
             | Cst.Stmt.AugDef (bind, expr) =>
               expandDef (fn (pos, var, expr) => Def (pos, LVar.Aug var, expr))
                         bind expr
        end

    and expandBlock (stmts, expr) = (VectorExt.flatMap expandStmt stmts, expandExpr expr)

    val desugar = expandBlock
end
