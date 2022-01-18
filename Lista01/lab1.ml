
let x_1 (a, b, c, d) = a;;
let x_2 (a, b, c, d) = b;;
let x_3 (a, b, c, d) = c;;
let x_4 (a, b, c, d) = d;;

let mirror4 xss =
  (x_2 xss, x_1 xss, x_4 xss, x_3 xss);;

mirror4 (1, 2, 3, 4);;

let rec substitute (xss, a, b) =
  if xss = [] then []
  else if List.hd xss = a then b :: substitute (List.tl xss, a, b)
  else List.hd xss :: substitute (List.tl xss, a, b);;

substitute ([1; 2; 3; 4; 5], 3, 8);;


let rec remove (xss, pos) =
  if pos = 0 then List.tl xss
  else List.hd xss :: remove (List.tl xss, pos - 1);;

remove ([1; 2; 3; 4; 5], 3);;
remove ([1; 2; 3; 4; 5], 8);;

