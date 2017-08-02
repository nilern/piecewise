structure DesugarAugs :> sig
    exception ReAssignment of Pos.t * AVar.t

    val desugar : (Ast.expr, Ast.stmt) Block.t -> (AuglessAst.expr, AuglessAst.stmt) Block.t

    end = struct

    exception ReAssignment of Pos.t * AVar.t

    structure Env :> sig
        type t

        val empty : t
        val lookup : t -> AVar.t -> (AVar.t option * int)
        val init : t -> CVar.t -> int -> t
        val insert : t -> CVar.t -> AVar.t -> t
    end = struct
        structure Map = BinaryMapFn(type ord_key = AVar.t
                                    val compare = AVar.compare)

        type t = (AVar.t option * int) Map.map

        val empty = Map.empty

        fun lookup env key = Map.lookup (env, key)

        fun init env var i = Map.insert (env, AVar.fromCVar var, (NONE, i))

        fun insert env var var' =
            let val avar = AVar.fromCVar var
                val (_, fi) = lookup env avar
            in Map.insert (env, avar, (SOME var', fi))
            end
    end (* structure Env *)

    val FixE = AuglessAst.FixE
    val FixS = AuglessAst.FixS
    val Prolog = AuglessAst.Prolog
    val Fn = Expr.Fn
    val Block = Expr.Block
    val PrimApp = Expr.PrimApp
    val Triv = Expr.Triv
    val Def = AuglessStmt.Def
    val Guard = AuglessStmt.Guard
    val Expr = AuglessStmt.Expr
    val Var = ATriv.Var
    val Const = ATriv.Const

    fun analyzeStmt (i, Ast.FixS stmt, env) =
        case stmt
        of AStmt.Def (_, var, _) => Env.init env var i
         | AStmt.AugDef (_, var, _) => Env.init env var i
         | AStmt.Guard _ => env

    fun elabExpr (Ast.FixE expr) =
        FixE (case expr
              of Expr.Fn (pos, name, params, cases) =>
                 Fn (pos, name, params, Vector.map elabCase cases)
               | Expr.Block (pos, block) => Block (pos, elabBlock block)
               | Expr.PrimApp (pos, po, args) => PrimApp (pos, po, Vector.map elabExpr args)
               | Expr.Triv (pos, t) => Triv (pos, t))

    (* OPTIMIZE: can actually use lexicals for all temporaries created here: *)
    and elabStmt (i, Ast.FixS stmt, (stmts', env)) =
        case stmt
        of AStmt.Def (pos, var, expr) =>
           let val avar = AVar.fromCVar var
               val (envVar, fi) = Env.lookup env avar
           in
               if Option.isSome envVar
               then raise ReAssignment (pos, avar)
               else let val (var', env') =
                            if i = fi
                            then (var, env)
                            else let val var' = CVar.fresh var
                                 in (var', Env.insert env var (AVar.fromCVar var'))
                                 end
                        val stmt' = FixS (Def (pos, var', elabExpr expr))
                    in (VectorExt.conj stmts' stmt', env')
                    end
           end
         | AStmt.AugDef (pos, var, expr) =>
           let val avar = AVar.fromCVar var
               val (envVar, fi) = Env.lookup env avar
               val (var', env') =
                   if i = fi
                   then (var, env)
                   else let val var' = CVar.fresh var
                        in (var', Env.insert env var (AVar.fromCVar var'))
                        end
               val oldVar = getOpt (envVar, valOf (AVar.upper avar))
               val ovExpr = FixE (Triv (pos, Var oldVar))
               val fnMerge = FixE (Expr.Triv (pos, Var (ATag.Lex, Name.fromString "fnMerge")))
               val merge = FixE (AuglessAst.app (pos, fnMerge,
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
end (* structure DesugarAugs *)
