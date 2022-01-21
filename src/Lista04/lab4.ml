(* Zadanie 1 *)

type 'a bt = Empty | Node of 'a * 'a bt * 'a bt * 'a bt;;

let rec mapTree3 = fun f -> fun bt ->
    match bt with
    | Empty -> Empty
    | Node(value, t1, t2, t3) -> Node(f value, mapTree3 f t1, mapTree3 f t2, mapTree3 f t3)

let tt = Node(1,
            Node(2,
                 Empty,
                 Empty,
                 Node(3,
                      Empty,
                      Empty,
                      Empty)
                ),
            Node(4,
                 Empty,
                 Node(5, Empty, Empty, Empty),
                 Node(7, Empty, Empty, Empty)
            ),
            Empty);;

mapTree3(fun x -> x+x)(tt);;


(* Zadanie 2 *)


type file = File of string;;

type catalog = Catalog of string * file list * catalog list;;

type disc = Disc of char * catalog list * file list;;



