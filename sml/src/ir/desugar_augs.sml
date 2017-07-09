structure DesugarAugs :> sig
exception ReAssignment of Pos.t * Var.t

val desugar : Ast.stmt vector -> AuglessAst.stmt vector

end = struct

exception ReAssignment of Pos.t * Var.t

structure Env :> sig
    type t

    val empty : t
    val lookup : t -> Var.t -> (Var.t option * int)
    val init : t -> Var.t -> int -> t
    val insert : t -> Var.t -> Var.t -> t
end = struct
    structure Map = BinaryMapFn(type ord_key = Var.t
                                val compare = Var.compare)

    type t = (Var.t option * int) Map.map

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

fun elabExpr (Ast.FixE expr) =
    FixE (case expr
          of Expr.Fn (pos, name, cases) =>
             let fun elabCase (prologue, body) =
                     (elabStmts prologue, elabExpr body)
             in Expr.Fn (pos, name, Vector.map elabCase cases)
             end
           | Expr.Block (pos, stmts) =>
             Expr.Block (pos, elabStmts stmts)
           | Expr.App (pos, f, args) =>
             Expr.App (pos, elabExpr f, Vector.map elabExpr args)
           | Expr.PrimApp (pos, po, args) =>
             Expr.PrimApp (pos, po, Vector.map elabExpr args)
           | Expr.Var (pos, v) => Expr.Var (pos, v)
           | Expr.Const (pos, c) => Expr.Const (pos, c))

and elabStmts stmts =
    let fun analyzeStmt (i, Ast.FixS stmt, env) =
            case stmt
            of AStmt.Def (_, var, _) => Env.init env var i
             | AStmt.AugDef (_, var, _) => Env.init env var i
             | AStmt.Guard _ => env
             | AStmt.Expr _ => env
        fun elabStmt (i, Ast.FixS stmt, (stmts', env)) =
            case stmt
            of AStmt.Def (pos, var, expr) =>
               let val (envVar, fi) = Env.lookup env var
               in
                   if Option.isSome envVar
                   then raise ReAssignment (pos, var)
                   else let val (var', env') =
                                if i = fi
                                then (var, env)
                                else let val var' = Var.fresh var
                                     in (var', Env.insert env var var')
                                     end
                            val stmt' =
                                FixS (AuglessVarStmt.Def (pos, var', elabExpr expr))
                        in (VectorExt.conj stmts' stmt', env')
                        end
               end
             | AStmt.AugDef (pos, var, expr) =>
               let val (envVar, fi) = Env.lookup env var
                   val (var', env') =
                       if i = fi
                       then (var, env)
                       else let val var' = Var.fresh var
                            in (var', Env.insert env var var')
                            end
                   val oldVar = Option.getOpt (envVar, Var.upper var)
                   val ovExpr = FixE (Expr.Var (pos, oldVar))
                   val fnMerge =
                       FixE (Expr.Var (pos,
                                        Var.Lex (Name.fromString "fnMerge")))
                   val merge =
                       FixE
                           (Expr.App (pos, fnMerge,
                                       Vector.fromList [ovExpr, elabExpr expr]))
                   val stmt' = FixS (AuglessVarStmt.Def (pos, var', merge))
               in (VectorExt.conj stmts' stmt', env')
               end
             | AStmt.Guard (pos, dnf) =>
               let val stmt' = FixS (AuglessVarStmt.Guard (pos, DNF.map elabExpr dnf))
               in (VectorExt.conj stmts' stmt', env)
               end
             | AStmt.Expr expr =>
               let val stmt' = FixS (AuglessVarStmt.Expr (elabExpr expr))
               in (VectorExt.conj stmts' stmt', env)
               end
        val env = Vector.foldli analyzeStmt Env.empty stmts
    in
        #1 (Vector.foldli elabStmt (VectorExt.empty (), env) stmts)
    end

val desugar = elabStmts
end (* structure DesugarAugs *)
