(*Pattern matching*)

fun fibonacci 0 = 0  (* Base case *)
  | fibonacci 1 = 1  (* Base case *)
  | fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)  (* Recursive case *)

fun sumList nil = 0 (*Base case*)
  | sumList (x::xs) = x + sumList(xs); (*for a list of this pattern, we can operate like this*)

(*or in this way*)
fun sumList(li) = 
    case li of
    [] => 0
    | x::xs => x + sumList(xs);

(*for more complicated stuff*)
(*check if the elements in this list are divisible by some integer*)
fun indivisible (num, li) = 
    case li of
    [] => true
    | x::xs => if x mod num = 0 then false
                else indivisible(num, xs);

(*check if elements in one list is in order*)
fun inorder li =
    case li of
    [] => true
    | x::[] => true
    | x::(y::ys) => if x > y then false else inorder(y::ys);

(*manipulate tuples: adding the tuples in the list to get (sum of the first element, sum of the second element) *)
fun addTuples (li: (int * int) list) = 
    case li of
    [] => (0,0)
    | x::[] => x
    | x::xs => (#1(x) + #1(addTuples(xs)), #2(x) + #2(addTuples(xs)));

