module type MEMORY = 
    sig
        type t = {n: int; mutable a: int option array}
        val init: int -> t
        val get: int -> t -> int option
        val set: int -> int -> t -> unit
        val dump: t -> int option array
    end;;

module ArrayMemory: MEMORY =
    struct
        type t = {n: int; mutable a: int option array}
        let init size = {n = size; a = Array.make size None}
        let get i memory = if i < memory.n && i >= 0 then memory.a.(i)
                        else None
        let set i elem memory = if i >= 0 && i < memory.n then memory.a.(i) <- Some(elem)
        let dump memory = memory.a

    end;;

let s = ArrayMemory.init 5;;

ArrayMemory.set 0 1 s;;
ArrayMemory.set 1 2 s;;
ArrayMemory.set 2 3 s;;
ArrayMemory.set 3 4 s;;
ArrayMemory.set 4 5 s;;
ArrayMemory.set 5 6 s;;

ArrayMemory.dump s;;

ArrayMemory.get (-1) s;;