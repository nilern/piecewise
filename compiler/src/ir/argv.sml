structure Argv0 = struct
    val op^^ = PPrint.^^
    val op<+> = PPrint.<+>

    type t = {self: Name.t, params: Name.t}

    fun toDoc {self = self, params = params} =
        PPrint.parens (PPrint.text "self =" <+> Name.toDoc self ^^ PPrint.text "," <+>
                           PPrint.text "params =" <+> Name.toDoc params)
end

structure Argv1 = struct
    val op^^ = PPrint.^^
    val op<+> = PPrint.<+>

    type t = {self: Name.t, params: Name.t, denv: Name.t}

    fun toDoc {self = self, params = params, denv = denv} =
        PPrint.parens (PPrint.text "self =" <+> Name.toDoc self ^^ PPrint.text "," <+>
                           PPrint.text "params =" <+> Name.toDoc params ^^ PPrint.text "," <+>
                               PPrint.text "denv =" <+> Name.toDoc denv)
end
