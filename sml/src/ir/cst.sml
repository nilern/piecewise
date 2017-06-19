structure CST = struct
    datatype expr = Fn of Pos.t * fnCase vector
                  | Block of Pos.t * stmt vector
                  | App of Pos.t * expr * expr vector
                  | PrimApp of Pos.t * Primop.t * expr vector
                  | Var of Pos.t * Var.t
                  | Const of Pos.t * Const.t

    and stmt = Def of expr * expr
             | AugDef of expr * expr
             | Expr of expr

    withtype fnCase = expr vector * expr option * expr

    fun exprPos (Fn (pos, _)) = pos
      | exprPos (Block (pos, _)) = pos
      | exprPos (App (pos, _, _)) = pos
      | exprPos (PrimApp (pos, _, _)) = pos
      | exprPos (Var (pos, _)) = pos
      | exprPos (Const (pos, _)) = pos

    fun stmtPos (Def (pat, _)) = exprPos pat
      | stmtPos (AugDef (pat, _)) = exprPos pat
      | stmtPos (Expr expr) = exprPos expr

    fun exprToString (Fn (_, cases)) =
            Vector.foldl (fn (c, acc) => acc ^ ";\n" ^ caseToString c)
                         "{" cases ^ "}"
      | exprToString (Block (_, stmts)) =
              Vector.foldl (fn (c, acc) => acc ^ ";\n" ^ stmtToString c)
                           "{" stmts ^ "}"
      | exprToString (App (_, f, args)) =
            "(" ^
                Vector.foldl (fn (arg, acc) => acc ^ " " ^ exprToString arg)
                             (exprToString f) args ^
                ")"
      | exprToString (PrimApp (_, opp, args)) =
            "(" ^
                Vector.foldl (fn (arg, acc) => acc ^ " " ^ exprToString arg)
                             (Primop.toString opp) args ^
                ")"
      | exprToString (Var (_, v)) = Var.toString v
      | exprToString (Const (_, c)) = Const.toString c

    and stmtToString (Def (pat, expr)) =
          exprToString pat ^ " = " ^ exprToString expr
      | stmtToString (AugDef (pat, expr)) =
            exprToString pat ^ " += " ^ exprToString expr
      | stmtToString (Expr expr) = exprToString expr

    and caseToString (pats, SOME cond, body) =
            Vector.foldl (fn (pat, acc) => acc ^ " " ^ exprToString pat)
                         "" pats ^ " | " ^ exprToString cond ^ " => " ^
                         exprToString body
      | caseToString (pats, NONE, body) =
              Vector.foldl (fn (pat, acc) => acc ^ " " ^ exprToString pat)
                           "" pats ^ " => " ^ exprToString body
end
