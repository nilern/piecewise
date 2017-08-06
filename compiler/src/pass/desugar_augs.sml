structure DesugarAugs :> sig
    exception ReAssignment of Pos.t * RVar.t

    val desugar : (Ast.expr, Ast.stmt) Block.t -> (AuglessAst.expr, AuglessAst.stmt) Block.t
end = struct
    structure Expr = AuglessAst.Expr
    structure Stmt = AuglessAst.Stmt
    structure Triv = Expr.Triv

    structure AExpr = Ast.Expr
    structure AStmt = Ast.Stmt
    structure ATriv = AExpr.Triv

    val FixE = AuglessAst.FixE
    val FixS = AuglessAst.FixS
    val Prolog = AuglessAst.Prolog
    val Fn = Expr.Fn
    val Block = Expr.Block
    val Call = Expr.Call
    val PrimCall = Expr.PrimCall
    val Triv = Expr.Triv
    val Def = Stmt.Def
    val Guard = Stmt.Guard
    val Expr = Stmt.Expr
    val Var = Triv.Var
    val Const = Triv.Const

    exception ReAssignment of Pos.t * RVar.t

    structure Env :> sig
        type t

        val empty : t
        val lookup : t -> RVar.t -> (RVar.t option * int)
        val init : t -> BaseVar.t -> int -> t
        val insert : t -> BaseVar.t -> RVar.t -> t
    end = struct
        structure Map = BinaryMapFn(type ord_key = RVar.t
                                    val compare = RVar.compare)

        type t = (RVar.t option * int) Map.map

        val empty = Map.empty

        fun lookup env key = Map.lookup (env, key)

        fun init env var i = Map.insert (env, RVar.fromBaseVar var, (NONE, i))

        fun insert env var var' =
            let val rv = RVar.fromBaseVar var
                val (_, fi) = lookup env rv
            in Map.insert (env, rv, (SOME var', fi))
            end
    end (* structure Env *)

    fun analyzeStmt (i, Ast.FixS stmt, env) =
        case stmt
        of AStmt.Def (_, LVar.Def var, _) => Env.init env var i
         | AStmt.Def (_, LVar.Aug var, _) => Env.init env var i
         | AStmt.Guard _ => env
         | AStmt.Expr _ => env

    fun elabExpr (Ast.FixE expr) =
        FixE (case expr
              of AExpr.Fn (pos, name, params, cases) =>
                 Fn (pos, Option.map RVar.fromBaseVar name, params, Vector.map elabCase cases)
               | AExpr.Block (pos, block) => Block (pos, elabBlock block)
               | AExpr.Call (pos, f, args) => Call (pos, elabExpr f, Vector.map elabExpr args)
               | AExpr.PrimCall (pos, po, args) => PrimCall (pos, po, Vector.map elabExpr args)
               | AExpr.Triv (pos, ATriv.Var v) => Triv (pos, Var (RVar.fromBaseVar v))
               | AExpr.Triv (pos, ATriv.Const c) => Triv (pos, Const c))

    (* OPTIMIZE: can actually use lexicals for all temporaries created here: *)
    and elabStmt (i, Ast.FixS stmt, (stmts', env)) =
        case stmt
        of AStmt.Def (pos, LVar.Def var, expr) =>
           let val avar = RVar.fromBaseVar var
               val (envVar, fi) = Env.lookup env avar
           in
               if Option.isSome envVar
               then raise ReAssignment (pos, avar)
               else let val (var', env') =
                            if i = fi
                            then (var, env)
                            else let val var' = BaseVar.fresh var
                                 in (var', Env.insert env var (RVar.fromBaseVar var'))
                                 end
                        val stmt' = FixS (Def (pos, var', elabExpr expr))
                    in (VectorExt.conj stmts' stmt', env')
                    end
           end
         | AStmt.Def (pos, LVar.Aug var, expr) =>
           let val (envVar, fi) = Env.lookup env (RVar.fromBaseVar var)
               val (var', env') =
                   if i = fi
                   then (var, env)
                   else let val var' = BaseVar.fresh var
                        in (var', Env.insert env var (RVar.fromBaseVar var'))
                        end
               val oldVar = getOpt (envVar, RVar.upper var)
               val ovExpr = FixE (Triv (pos, Var oldVar))
               val fnMerge =
                   FixE (Expr.Triv (pos,
                                    Var (RVar.Current (BaseVar.Lex (Name.fromString "fnMerge")))))
               val merge = FixE (Call (pos, fnMerge, (* TODO: via `apply` *)
                                       Vector.fromList [ovExpr, elabExpr expr]))
               val stmt' = FixS (Def (pos, var', merge))
           in (VectorExt.conj stmts' stmt', env')
           end
         | AStmt.Guard (pos, dnf) =>
           let val stmt' = FixS (Guard (pos, DNF.map elabExpr dnf))
           in (VectorExt.conj stmts' stmt', env)
           end
         | AStmt.Expr expr =>
           let val stmt' = FixS (Expr (elabExpr expr))
           in (VectorExt.conj stmts' stmt', env)
           end

    and elabBlock (stmts, expr) =
        let val env = Vector.foldli analyzeStmt Env.empty stmts
        in (#1 (Vector.foldli elabStmt (VectorExt.empty (), env) stmts), elabExpr expr)
        end

    and elabCase (Ast.Prolog (cond, bindStmts), body) =
        let val cond' = DNF.map elabExpr cond
            val (bindStmts', body') = elabBlock (bindStmts, body)
        in (Prolog (cond', bindStmts'), body')
        end

    val desugar = elabBlock
end
