 open QCheck

let rec member xs y = match xs with
   | [] -> false
   | x::xs -> x=y || member xs y;;

(*member (Gen.(generate1 (list_size (int_bound 1_000_000) small_int))) 200000;;*)

 let rec fac n = match n with
   | 0 -> 1
   | _ -> n * fac (n-1)
 
 let rec reverse xs = match xs with
   | [] -> []
   | x::xs -> (reverse xs) @ [x]

let fac' n =
    let rec fac_local n acc = match n with
        | 0 -> acc
        | _ -> fac_local (n-1) (acc * n)
    in fac_local n 1

let fac_test = Test.make ~name:"Factorial Test" ~count:1000 (int_bound 1_000_000) (
    fun (x) -> fac' x > -1 = true) 

let fac_test2 = Test.make ~name:"Factorial Test" ~count:1000 (int_bound 1_000_000) (
    fun (x) -> fac x > -1 = true) 

let _ = QCheck_runner.run_tests ~verbose:true [fac_test;fac_test2]