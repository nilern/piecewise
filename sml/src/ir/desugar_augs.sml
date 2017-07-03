structure DesugarAugs :> sig
exception ReAssignment of Pos.t * Var.t

val desugar : Ast.stmt vector -> AuglessAst.stmt vector

end = struct

exception ReAssignment of Pos.t * Var.t

structure Env :> sig
    type t

    val empty : t
    val lookup : t -> Var.t -> (Var.t option * int * int)
    val init : t -> Var.t -> int -> int -> t
    val insert : t -> Var.t -> Var.t -> t
end = struct
    structure Map = BinaryMapFn(type ord_key = Var.t
                                val compare = Var.compare)

    type t = (Var.t option * int * int) Map.map

    val empty = Map.empty

    fun lookup env key = Map.lookup (env, key)

    fun init env var i j = Map.insert (env, var, (NONE, i, j))

    fun insert env var var' =
        let val (_, fi, fj) = lookup env var
        in Map.insert (env, var, (SOME var', fi, fj))
        end
end (* structure Env *)

val FixE = AuglessAst.FixE
val FixS = AuglessAst.FixS
val FixBS = AuglessAst.FixBS

fun analyzeBind env i (Ast.Bind (pos, _, bstmts)) =
    let fun analyzeBStmt (j, Ast.FixBS stmt, env) =
            case stmt
            of CStmt.Def (var, _) => Env.init env var i j
             | CStmt.AugDef (var, _) => Env.init env var i j
             | CStmt.Expr _ => env
    in Vector.foldli analyzeBStmt env bstmts
    end

fun elabBind env i (Ast.Bind (pos, dnf, bstmts)) =
    let fun elabBStmt (j, Ast.FixBS stmt, (stmts', env)) =
            case stmt
            of CStmt.Def (var, expr) =>
               let val (envVar, fi, fj) = Env.lookup env var
               in
                   if Option.isSome envVar
                   then raise ReAssignment (pos, var)
                   else let val (var', env') =
                                if i = fi andalso j = fj
                                then (var, env)
                                else let val var' = Var.fresh var
                                     in (var', Env.insert env var var')
                                     end
                            val stmt' =
                                FixBS (BindStmt1.Def (var', elabExpr expr))
                        in (VectorExt.conj stmts' stmt', env')
                        end
               end
             | CStmt.AugDef (var, expr) =>
               let val (envVar, fi, fj) = Env.lookup env var
                   val (var', env') =
                       if i = fi andalso j = fj
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
                   val stmt' = FixBS (BindStmt1.Def (var', merge))
               in (VectorExt.conj stmts' stmt', env')
               end
             | CStmt.Expr expr =>
               let val stmt' = FixBS (BindStmt1.Expr (elabExpr expr))
               in (VectorExt.conj stmts' stmt', env)
               end
        val (bstmts', env') =
            Vector.foldli elabBStmt (VectorExt.empty (), env) bstmts
    in (AuglessAst.Bind (pos, DNF.map elabExpr dnf, bstmts'), env')
    end

and elabExpr expr =
    FixE
        (case Ast.unwrapE expr
         of Expr.Fn (pos, name, cases) =>
            let fun elabCase (bind, body) =
                    let val env = analyzeBind Env.empty 0 bind
                        val (bind', _) = elabBind env 0 bind
                    in (bind', elabExpr body)
                    end
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
            of AuglessStmt.Def (_, bind, _) => analyzeBind env i bind
             | AuglessStmt.Expr _ => env
        fun elabStmt (i, Ast.FixS stmt, (stmts', env)) =
            case stmt
            of AuglessStmt.Def (temp, bind, expr) =>
               let val (bind', env') = elabBind env i bind
                   val stmt' = FixS (AuglessStmt.Def (temp, bind', elabExpr expr))
               in (VectorExt.conj stmts' stmt', env')
               end
             | AuglessStmt.Expr expr =>
               let val stmt' = FixS (AuglessStmt.Expr (elabExpr expr))
               in (VectorExt.conj stmts' stmt', env)
               end
        val env = Vector.foldli analyzeStmt Env.empty stmts
    in
        #1 (Vector.foldli elabStmt (VectorExt.empty (), env) stmts)
    end

val desugar = elabStmts
end (* structure DesugarAugs *)
