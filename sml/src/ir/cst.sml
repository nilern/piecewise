structure CST = struct
    datatype expr = Fn of fnCase vector
                  | Block of stmt vector
                  | App of expr * expr vector
                  | PrimApp of Primop.t * expr vector
                  | Var of Var.t
                  | Const of Const.t

    and stmt = Def of expr * expr
             | AugDef of expr * expr
             | Expr of expr

    withtype fnCase = expr vector * expr option * expr

    fun toString (Var v) = Var.toString v
end
