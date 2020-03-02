open QCheck

let dictionary = Dict.empty

let dictionary = Dict.add dictionary "Hello" 1;;

print_int (Dict.find dictionary "Hello");;
print_newline;;

let myshr i = if i = 0 then Iter.empty else Iter.return(int_of_float (sqrt (float_of_int (i))));;
let t = Test.make (set_shrink myshr int) (fun i -> i < 432);;
QCheck_runner.run_tests [t];;