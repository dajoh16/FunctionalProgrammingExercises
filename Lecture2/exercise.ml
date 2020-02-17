open QCheck

let rec msb n = 
  if n = 0
  then 0
  else 1 + msb(n lsr 1);;

let rec length l = match l with
  | [] -> 0
  | elem::elems -> 1 + length(elems)

let rec sum l = match l with
  | [] -> 0
  | elem::elems -> elem + sum(elems)

let rec member l num = match l with 
  | [] -> false
  | elem::elems -> if elem = num
    then true
    else member elems num

let fst n = match n with
    (x,y) -> x

let snd n = match n with
    (x,y) -> y

let sum_test = Test.make ~name:"Sum Test" ~count:10000 (pair (list int) (list int))
    (fun (xs,ys) -> sum (xs@ys) = sum xs + sum ys)

let _ = QCheck_runner.run_tests ~verbose:true [sum_test]

let rec merge_sort l1 l2 = match l1, l2 with
  |  [],_ -> l2
  | _,[] -> l1
  | elem1::elems1, elem2::elems2 -> if elem1 <= elem2 
    then elem1 :: merge_sort elems1 l2
    else elem2 :: merge_sort l1 elems2 

let merge_test = Test.make ~name:"Merge Test" ~count:1000 (pair (list int) (list int)) (
    fun (xs,ys) -> merge_sort (List.sort compare xs) (List.sort compare ys) = List.sort compare (xs@ys) ) 

let _ = QCheck_runner.run_tests ~verbose:true [merge_test]

let test1 = Test.make int (fun x -> x = Int64.to_int(Int64.of_int x))
let test2 = Test.make int (fun x -> x = Int32.to_int(Int32.of_int x))
let test3 = Test.make int (fun x -> x = int_of_string(string_of_int x))

let _ = QCheck_runner.run_tests ~verbose:true [test1;test2;test3]