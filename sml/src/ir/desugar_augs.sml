structure DesugarAugs :> sig
exception ReAssignment of Pos.t * Var.t

val desugar : DnfCst.stmt vector -> AuglessCst.stmt vector

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

val FixE = AuglessCst.FixE
val FixS = AuglessCst.FixS
val FixBS = AuglessCst.FixBS

fun analyzeBind env i (DnfCst.Bind (pos, _, bstmts)) =
    let fun analyzeBStmt (j, DnfCst.FixBS stmt, env) =
            case stmt
            of Stmt0.Def (var, _) => Env.init env var i j
             | Stmt0.AugDef (var, _) => Env.init env var i j
             | Stmt0.Expr _ => env
    in Vector.foldli analyzeBStmt env bstmts
    end

fun elabBind env i (DnfCst.Bind (pos, dnf, bstmts)) =
    let fun elabBStmt (j, DnfCst.FixBS stmt, (stmts', env)) =
            case stmt
            of Stmt0.Def (var, expr) =>
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
             | Stmt0.AugDef (var, expr) =>
               let val (envVar, fi, fj) = Env.lookup env var
                   val (var', env') =
                       if i = fi andalso j = fj
                       then (var, env)
                       else let val var' = Var.fresh var
                            in (var', Env.insert env var var')
                            end
                   val oldVar = Option.getOpt (envVar, Var.upper var)
                   val ovExpr = FixE (Expr0.Var (pos, oldVar))
                   val fnMerge =
                       FixE (Expr0.Var (pos,
                                        Var.Lex (Name.fromString "fnMerge")))
                   val merge =
                       FixE
                           (Expr0.App (pos, fnMerge,
                                       Vector.fromList [ovExpr, elabExpr expr]))
                   val stmt' = FixBS (BindStmt1.Def (var', merge))
               in (VectorExt.conj stmts' stmt', env')
               end
             | Stmt0.Expr expr =>
               let val stmt' = FixBS (BindStmt1.Expr (elabExpr expr))
               in (VectorExt.conj stmts' stmt', env)
               end
        val (bstmts', env') =
            Vector.foldli elabBStmt (VectorExt.empty (), env) bstmts
    in (AuglessCst.Bind (pos, DNF.map elabExpr dnf, bstmts'), env')
    end

and elabExpr expr =
    FixE
        (case DnfCst.unwrapE expr
         of Expr0.Fn (pos, name, cases) =>
            let fun elabCase (bind, body) =
                    let val env = analyzeBind Env.empty 0 bind
                        val (bind', _) = elabBind env 0 bind
                    in (bind', elabExpr body)
                    end
            in Expr0.Fn (pos, name, Vector.map elabCase cases)
            end
          | Expr0.Block (pos, stmts) =>
            Expr0.Block (pos, elabStmts stmts)
          | Expr0.App (pos, f, args) =>
            Expr0.App (pos, elabExpr f, Vector.map elabExpr args)
          | Expr0.PrimApp (pos, po, args) =>
            Expr0.PrimApp (pos, po, Vector.map elabExpr args)
          | Expr0.Var (pos, v) => Expr0.Var (pos, v)
          | Expr0.Const (pos, c) => Expr0.Const (pos, c))

and elabStmts stmts =
    let fun analyzeStmt (i, DnfCst.FixS stmt, env) =
            case stmt
            of Stmt1.Def (_, bind, _) => analyzeBind env i bind
             | Stmt1.Expr _ => env
        fun elabStmt (i, DnfCst.FixS stmt, (stmts', env)) =
            case stmt
            of Stmt1.Def (temp, bind, expr) =>
               let val (bind', env') = elabBind env i bind
                   val stmt' = FixS (Stmt1.Def (temp, bind', elabExpr expr))
               in (VectorExt.conj stmts' stmt', env')
               end
             | Stmt1.Expr expr =>
               let val stmt' = FixS (Stmt1.Expr (elabExpr expr))
               in (VectorExt.conj stmts' stmt', env)
               end
        val env = Vector.foldli analyzeStmt Env.empty stmts
    in
        #1 (Vector.foldli elabStmt (VectorExt.empty (), env) stmts)
    end

val desugar = elabStmts
end (* structure DesugarAugs *)
