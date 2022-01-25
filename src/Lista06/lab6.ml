let count arr num =
    let i = ref 0
    in 
    let counter = ref 0
        in while !i < (Array.length arr) do
            if arr.(!i) = num then counter := !counter + 1;
            i := !i + 1;
        done;
        !counter
    ;;

count [|1; 2; 3; 1; 4; 5; 3; 7; 1; 11; 2|] 1;;
count [||] 1;;
count [|2; 3; 4|] 1;;


