structure DesugarAugs :> sig
    exception ReAssignment of Pos.t * AVar.t

    val desugar : (Ast.expr, Ast.stmt) Block.t -> (AuglessAst.expr, AuglessAst.stmt) Block.t

    end = struct

    exception ReAssignment of Pos.t * AVar.t

    structure Env :> sig
        type t

        val empty : t
        val lookup : t -> AVar.t -> (AVar.t option * int)
        val init : t -> AVar.t -> int -> t
        val insert : t -> AVar.t -> AVar.t -> t
    end = struct
        structure Map = BinaryMapFn(type ord_key = AVar.t
                                    val compare = AVar.compare)

        type t = (AVar.t option * int) Map.map

        val empty = Map.empty

        fun lookup env key = Map.lookup (env, key)

        fun init env var i = Map.insert (env, var, (NONE, i))

        fun insert env var var' =
            let val (_, fi) = lookup env var
            in Map.insert (env, var, (SOME var', fi))
            end
    end (* structure Env *)

    val FixE = AuglessAst.FixE
    val FixS = AuglessAst.FixS
    val Fn = Expr.Fn
    val Block = Expr.Block
    val PrimApp = Expr.PrimApp
    val Triv = Expr.Triv
    val Def = AuglessVarStmt.Def
    val Guard = AuglessVarStmt.Guard
    val Expr = AuglessVarStmt.Expr
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
                 Fn (pos, name, params, Vector.map elabBlock cases)
               | Expr.Block (pos, block) => Block (pos, elabBlock block)
               | Expr.PrimApp (pos, po, args) => PrimApp (pos, po, Vector.map elabExpr args)
               | Expr.Triv (pos, t) => Triv (pos, t))

    and elabStmt (i, Ast.FixS stmt, (stmts', env)) =
        case stmt
        of AStmt.Def (pos, var, expr) =>
           let val (envVar, fi) = Env.lookup env var
           in
               if Option.isSome envVar
               then raise ReAssignment (pos, var)
               else let val (var', env') =
                            if i = fi
                            then (var, env)
                            else let val var' = AVar.fresh var
                                 in (var', Env.insert env var var')
                                 end
                        val stmt' = FixS (Def (pos, var', elabExpr expr))
                    in (VectorExt.conj stmts' stmt', env')
                    end
           end
         | AStmt.AugDef (pos, var, expr) =>
           let val (envVar, fi) = Env.lookup env var
               val (var', env') =
                   if i = fi
                   then (var, env)
                   else let val var' = AVar.fresh var
                        in (var', Env.insert env var var')
                        end
               val oldVar = getOpt (envVar, valOf (AVar.upper var))
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

    val desugar = elabBlock
end (* structure DesugarAugs *)
