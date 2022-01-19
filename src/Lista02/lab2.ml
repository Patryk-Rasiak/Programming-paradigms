(* Zadanie 1 *)

let rec find = function xs -> function x ->
                 match xs with
                 | h::t when h = x -> true
                 | [] -> false
                 | h::t -> find t x
                              
;;

let find123 = find [1; 2; 3];;
let findxyz = find ['x'; 'y'; 'z'];;
let findNothing = find [];;

find123 4 = false;;
find123 3 = true;;
findxyz 'y' = true;;
findxyz 'a' = false;;
findNothing 3 = false;;

(* Zadanie 2a *)

let rec split3Rec xs =
    match xs with
        [] -> ([], [], [])
        | [x] -> ([x], [], [])
        | [x; y] -> ([x], [y], [])
        | h1 :: h2 :: h3 :: t ->
          let (first, second, third) = split3Rec t
          in (h1 :: first, h2 :: second, h3 :: third)
;;

split3Rec [1; 2; 3; 4; 5; 6];;
split3Rec [];;
split3Rec ['a'; 'b'; 'c'; 'd'];;

(* Zadanie 2b *)

let rec split3Tail xs =
    let rec split3TailIter(xs1, xs2, xs3, xs, acc) =

        match xs with
        | [] -> (xs1, xs2, xs3)
        | h::t when acc mod 3 = 0 -> split3TailIter(h::xs1, xs2, xs3, t, acc+1)
        | h::t when acc mod 3 = 1 -> split3TailIter(xs1, h::xs2, xs3, t, acc+1)
        | h::t when acc mod 3 = 2 -> split3TailIter(xs1, xs2, h::xs3, t, acc+1)

    in 
    split3TailIter([], [], [], xs, 0);;

split3Tail [1; 2; 3; 4; 5; 6];;
split3Tail [];;
split3Tail ['a'; 'b'; 'c'; 'd'];;