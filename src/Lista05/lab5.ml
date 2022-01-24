type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec lfrom k = LCons (k, function () -> lfrom (k + 1));;

let rec toLazyList = function
    | [] -> LNil
    | x :: xs -> LCons (x, function () -> (toLazyList xs));;

let rec ltake = function
    | (0, _) -> []
    | (_, LNil) -> []
    | (n, LCons (x, xsfunc)) -> x :: ltake (n-1, xsfunc());;


let rec buyTicket xs n =
    match (xs, n) with
    | ([], _) -> []
    | ((_, prizefun) :: t, 1) -> (prizefun(), prizefun) :: t
    | (h :: t, n) -> h :: (buyTicket t (n - 1));;


let tickets = [("?", function () -> "komputer"); ("?", function () -> "laptop"); ("?", function () -> "dziurawe skarpetki"); ("?", function () -> "zdechly pies")];;
let empty = [];;

buyTicket tickets 1;;
buyTicket (buyTicket tickets 3) 4;;
buyTicket empty 2;;



