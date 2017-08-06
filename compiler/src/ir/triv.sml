signature TRIV = sig
    structure Var: TO_DOC
    structure Const: TO_DOC

    datatype t = Var of Var.t
               | Const of Const.t

    val toDoc : t -> PPrint.doc
end

functor TrivFn(structure V: TO_DOC structure C: TO_DOC) : TRIV = struct
    structure Var = V
    structure Const = C

    datatype t = Var of Var.t
               | Const of Const.t

    val toDoc = fn Var v => Var.toDoc v
                 | Const c => Const.toDoc c
end
