(* Question 1 *)
(* TODO: Write your own tests for the fact function.
         Remember that you should NOT test cases for n < 0.
*)
(* TODO: Correct these tests for the fact function. *)
let fact_tests = [
  (0, 1.);
  (1, 1.);
  (2, 2.);
  (5, 120.);
  (6, 720.);
  (7, 5040.);
  (10, 3628800.)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec fact (n: int): float = 
  match n with
  | 0 -> 1.
  | n -> float_of_int (n) *. (fact(n - 1))
;;


(* TODO: Write your own tests for the binomial function.
         See the provided tests for fact, above, for how to write test cases.
         Remember that we assume that  n >= k >= 0; you should not write test cases where this assumption is violated.
*)
let binomial_tests = [
  (* Your test cases go here. Correct the incorrect test cases for the function. *)
  ((0, 0), 1.);
  ((1, 0), 1.);
  ((2, 0), 1.);
  ((10, 1), 10.);
  ((10, 2), 45.);
  ((5, 5), 1.);
  ((4, 3), 4.);
  ((20, 10), 184756.)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)

    (*besoin de régler le overflow*)
let binomial (n: int) (k: int) =
  if n < 0
  then domain ()
  else (if k > n
        then domain()
        else fact (n) /. (fact k *. fact (n - k)))


(* TODO: Write a good set of tests for distance. *)
let distance_tests = [ 
    (* Your test cases go here *)
  ((5, 4), (6, 3)), 1.414213562;
  ((1, 2), (3, 4)), 2.82842712474619;
  ((5, 2), (1, 1)), 4.123105623;
  ((7, 1), (4, 1000)), 999.00450449435 ;
  ((3, 3), (3, 3)),  0.
]
;;

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let distance ((x1, y1): (int * int)) ((x2, y2): (int * int)) : float =
  let dx = (x1 - x2) in
  let dy = (y1 - y2) in
  sqrt(float_of_int((dx * dx) + (dy * dy)))
;;


(* Question 2: is_prime *)

(* TODO: Write a good set of tests for is_prime. *)
let is_prime_tests = [ 
(* Your tests go here *)
  (2, true);
  (3, true);
  (4, false);
  (5, true);
  (6, false);
  (7, true)

]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)


let is_prime n =
  if n <= 1
  then domain()
  else
    let rec not_divisor d =
      d*d > n || (n mod d <> 0 && not_divisor (d+1)) 
    in
    n >= 2 && not_divisor 2;;


(* Question 3: Riemann Zeta function 
    Implement the `approx_zeta` function.
    You do not need to modify any other parts inside the `zeta` function
*)

let zeta_tests = [
    (* Your tests go here *)
  (3., 1.2020569031595942853997381615114499907649862923404988817922715553);
  (4., 1.0823232337111381915160036965411679027747509519187269076829762154);
  (5., 1.0369277551433699263313654864570341680570809195019128119741926779)
] 
;;
  
let zeta (k: float) : float = 
  let rec approx_zeta k acc n sum_so_far = 
    if (1. /. (n ** k)) < acc
    then
      sum_so_far
    else
      let rec_val = ((1. /. (n**k)) +. sum_so_far) in
      approx_zeta k acc (n +. 1.) rec_val
      
        (*raise NotImplemented*)
  in
      (*  Note that we put < 2. while the function still works 
          to evaluate any smaller arguments *)
  if k < 2. 
  then domain () 
  else approx_zeta k epsilon_float 1. 0.
;;


(* Question 4: Fibonacci*)

(* TODO: Write a good set of tests for fib_tl. *)
let fib_tl_tests = [
  (0, 1);
  (1, 1);
  (2, 2);
  (3, 3);
  (4, 5);
  (5, 8)
]

(* TODO: Implement a tail-recursive helper fib_aux. *)
let rec fib_aux n a b =
  if n = 0
  then a
  else if n = 1
  then b
  else
    fib_aux (n-1) b (a+b)
;;
  (*raise NotImplemented*)
  
  
  

(* TODO: Implement fib_tl using fib_aux. *)
let fib_tl n =
  fib_aux n 1 1
;;
  (*raise NotImplemented*)

