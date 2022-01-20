let nComp f n = fun x ->
    if n <= 0 then []
    else
        let rec helper elem i =
            match i with
                0 -> []
                |_ -> elem::(helper (f elem) (i-1))
        in helper x n
    ;;

nComp (fun x -> x * x) 4 3 = [3; 9; 81; 6561];;
nComp (fun x -> x / 2) (-1) 8 = [];;
nComp (fun x -> x * x * x) 3 (-2) = [-2; -8; -512];;

let area (a, b) f n =
    let rec sieve from_n =
        if from_n <= n then from_n::sieve(from_n + 1) else [] in
    let interval_len = (b -. a) /. float(n)
    in List.fold_left (+.) 0. (List.map (fun x -> abs_float(f(a +. interval_len *. float(x))) *. interval_len) (sieve 1))
;;

let f (x: float): float = x *. x;;
let f1 (x: float): float = 4. -. x;;

area (0., 3.) f 100 = 9.13544999999999874;;
area (1.0, 2.0) f 1500 = 2.33433340740740913;;
area (0., 4.) f1 100000 = 7.99992000000000125;;
area (1., 2.) f1 2000 = 2.49975;

