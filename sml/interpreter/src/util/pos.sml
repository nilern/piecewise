structure Pos = struct
    type t = {file: string, index: int, line: int, col: int}

    fun start file = {file = file, index = 0, line = 1, col = 1}

    val def = {file = "__", index = 0, line = 1, col = 1}

    fun next { file, index, line, col} =
        fn #"\n" => { file, index = index + 1, line = line + 1, col = 1 }
         | _     => { file, index = index + 1, line = line, col = col + 1 }

    fun toString {file, index, line, col} =
            String.concat[file, " @ ", Int.toString line, ":", Int.toString col]
end
