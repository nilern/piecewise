(* TODO: formals is guaranteed to be a tuple so matching it can be optimized *)

structure DesugarBinds :> sig
    val desugar : (Cst.expr, Cst.stmt) Block.t -> (Ast.expr, Ast.stmt) Block.t

    exception Pattern of Pos.t * Cst.expr
end = struct
    val FixE = Ast.FixE
    val FixS = Ast.FixS
    val Fn = Expr.Fn
    val Block = Expr.Block
    val PrimApp = Expr.PrimApp
    val Triv = Expr.Triv
    val Def = AStmt.Def
    val AugDef = AStmt.AugDef
    val Guard = AStmt.Guard
    val Var = Triv0.Var
    val Const = Triv0.Const

    exception Pattern of Pos.t * Cst.expr

    fun expandPat newBinding pat access parentId (cond, binds) =
        case Cst.unwrapE pat
        of CExpr.Fn _ => raise Pattern (Cst.exprPos pat, pat)
         | CExpr.Block _ => raise Pattern (Cst.exprPos pat, pat)
         | CExpr.App (pos, f, args) =>
           let val unapply = FixE (Triv (pos, Var (Var.Lex (Name.fromString "unapply"))))
               val eq = FixE (Triv (pos, Var (Var.Lex (Name.fromString "=="))))
               val f' = expandExpr f
               val access' = FixE (Ast.app (pos, unapply, Vector.fromList [f', access]))
               val tag = FixE (Expr.PrimApp (pos, Primop.Tag, VectorExt.singleton access'))
               val some = FixE (Triv (pos, Const (Const.Symbol "Some")))
               val newCondExpr = FixE (Ast.app (pos, eq, Vector.fromList [tag, some]))
               val (newCond, parentId') = DNF.require newCondExpr (OptionExt.toList parentId)
               val cond' = DNF.conj (Vector.fromList [cond, newCond])
               val access'' = FixE (Expr.PrimApp (pos, Primop.Repr, Vector.fromList [access']))
               fun expandArg (i, arg) =
                   let val pos = Cst.exprPos arg
                       val ie = FixE (Triv (pos, Const (Const.Int (Int.toString i))))
                       val argAccess = FixE (Expr.PrimApp (pos, Primop.AGet,
                                                           Vector.fromList [access'', ie]))
                   in expandPat newBinding arg argAccess (SOME parentId')
                                (cond', VectorExt.empty ())
                   end
               val argCbs = Vector.mapi expandArg args
               val argConds = Vector.map #1 argCbs
               val cond'' = DNF.conj argConds
               fun step ((_, bs), acc) = VectorExt.concat acc bs
               val binds' = Vector.foldl step binds argCbs
           in (cond'', binds')
           end
         | CExpr.PrimApp (_, po, args) => raise Fail "unimplemented"
         | CExpr.Triv (_, Triv0.Var var) =>
           (cond, VectorExt.conj binds (newBinding (Cst.exprPos pat, var, access)))
         | CExpr.Triv (pos, Triv0.Const c) =>
           let val eq = FixE (Triv (pos, Var (Var.Lex (Name.fromString "=="))))
               val c' = FixE (Triv (pos, Const c))
               val newCondExpr = FixE (Ast.app (pos, eq, Vector.fromList [access, c']))
               val (newCond, _) = DNF.require newCondExpr (OptionExt.toList parentId)
           in
               ( DNF.conj (Vector.fromList [cond, newCond])
               , binds )
           end

    and expandBind newBinding (Cst.Bind (pat, cond)) access =
        let val (cond', bindStmts) =
                expandPat newBinding pat access NONE (DNF.always (), VectorExt.empty ())
        in
            if DNF.isAlways cond'
            then bindStmts
            else VectorExt.prepend bindStmts (FixS (Guard (Cst.exprPos pat, cond')))
        end

    and expandExpr (Cst.FixE expr) =
        FixE (case expr
              of CExpr.Fn (pos, name, cases) =>
                 let val params = Name.freshFromString "params"
                     val paccess = FixE (Triv (pos, Var (Var.Lex params)))
                     fun expandCase (prologue, body) =
                         (expandBind (FixS o Def) prologue paccess, expandExpr body)
                 in Fn (pos, name, params, Vector.map expandCase cases)
                 end
               | CExpr.Block (pos, block) => Block (pos, expandBlock block)
               | CExpr.App (pos, f, args) =>
                 Ast.app (pos, expandExpr f, Vector.map expandExpr args)
               | CExpr.PrimApp (pos, po, args) => PrimApp (pos, po, Vector.map expandExpr args)
               | CExpr.Triv (pos, Triv0.Var v) => Triv (pos, Var v)
               | CExpr.Triv (pos, Triv0.Const c) => Triv (pos, Const c))

    and expandStmt (Cst.FixS stmt) =
        let fun expandDef newDef (bind as Cst.Bind (pat, _)) expr =
                (* FIXME: tuple etc. pats get treated as fn definitions *)
                case bind
                of Cst.Bind (Cst.FixE (CExpr.App (pos, f, args)), cond) =>
                   let val name = case f
                                  of Cst.FixE (CExpr.Triv (_, Triv0.Var name)) => SOME name
                                   | _ => NONE
                       val tuple =
                           Cst.FixE (CExpr.Triv (pos, Var (Var.Lex (Name.fromString "tuple"))))
                       val ftup = Cst.FixE (CExpr.App (pos, tuple, args))
                       val cs = (Cst.Bind (ftup, cond), expr)
                   in expandDef newDef (Cst.Bind (f, NONE))
                                (Cst.FixE (CExpr.Fn (pos, name, VectorExt.singleton cs)))
                   end
                 | _ =>
                   (case expandExpr expr
                    of expr' as Ast.FixE (Expr.Triv _) => expandBind (FixS o newDef) bind expr'
                     | expr' => let val pos = Cst.exprPos pat
                                    val triv = Var.Lex (Name.freshFromString "v")
                                    val trivDef = FixS (Def (pos, triv, expr'))
                                    val trivUse = FixE (Expr.Triv (pos, Var triv))
                                    val bindStmts = expandBind (FixS o newDef) bind trivUse
                                in VectorExt.prepend bindStmts trivDef
                                end)
        in
            case stmt
            of CStmt.Def (bind, expr) => expandDef Def bind expr
             | CStmt.AugDef (bind, expr) => expandDef AugDef bind expr
        end

    and expandBlock (stmts, expr) = (VectorExt.flatMap expandStmt stmts, expandExpr expr)

    val desugar = expandBlock
end
