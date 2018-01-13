structure Dump :> sig
    type ('prompt, 'value, 'subcont) t

    val empty : ('p, 'v, 'k) t
    val push : ('p, 'v, 'k) t -> 'p option -> 'v Env.t -> 'k -> ('p, 'v, 'k) t
    val append : ('p, 'v, 'k) t -> ('p, 'v, 'k) t -> ('p, 'v, 'k) t

    val split : ('p, 'v, 'k) t -> 'p -> { top: ('p, 'v, 'k) t, bottom: ('p, 'v, 'k) t } option
    val prefix : ('p, 'v, 'k) t -> 'p -> ('p, 'v, 'k) t option
    val unwind : ('p, 'v, 'k) t -> 'p -> ('p, 'v, 'k) t option

    val find : ('p, 'v, 'k) t -> string -> 'v option
end = struct
    type ('p, 'v, 'k) frame = { prompt: 'p option, env: 'v Env.t, subcont: 'k }
    type ('p, 'v, 'k) t = ('p, 'v, 'k) frame list

    val empty = []

    fun push cont prompt env subcont =
        { prompt = prompt, env = env, subcont = subcont } :: cont

    fun append top bottom = top @ bottom

    fun split cont target =
        let fun loop ((frame as { prompt = prompt, env = env, subcont = subcont }) :: cont') top =
                (case prompt
                 of SOME prompt =>
                     if MLton.eq (prompt, target)
                     then SOME { top = List.rev top
                               , bottom = { prompt = NONE, env = env, subcont = subcont }
                                          :: cont' }
                     else loop cont' (frame :: top)
                  | NONE => loop cont' (frame :: top))
              | loop [] top = NONE
        in loop [] cont
        end

    fun prefix dump target = Option.map #top (split dump target)

    fun unwind cont target =
        let fun loop ((frame as { prompt = prompt, env = env, subcont = subcont }) :: cont') =
                (case prompt
                 of SOME prompt =>
                     if MLton.eq (prompt, target)
                     then SOME ({ prompt = NONE, env = env, subcont = subcont } :: cont')
                     else loop cont'
                  | NONE => loop cont')
              | loop [] = NONE
        in loop cont
        end

    fun find ({ prompt = _, env = env, subcont = _ } :: cont') name =
        OptionExt.orElse (Env.find env name) (fn () => find cont' name)
end
