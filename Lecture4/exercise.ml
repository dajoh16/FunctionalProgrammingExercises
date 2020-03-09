open QCheck

(* REMEMBER THIS FOR INFIX OPERATOR *)
let (<+>) = Iter.(<+>)

let dictionary = Dict.empty

let dictionary = Dict.add dictionary "Hello" 1;;

print_int (Dict.find dictionary "Hello");;
print_newline;;

let myshr i = if i = 0 then Iter.empty else Iter.return(int_of_float (sqrt (float_of_int (i))));;

let myshr2 i = match i with
    | 0 -> Iter.empty
    | _ -> (Iter.return (i/10))
     <+> (Iter.return (i/2))
     <+> (Iter.return (i-1))

let t = Test.make (set_shrink myshr2 int) (fun i -> i < 432);;

QCheck_runner.run_tests [t];;


