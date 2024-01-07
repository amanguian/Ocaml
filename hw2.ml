(* Section 1 : Lists *)

(* Question 1.1 : Most common element of *sorted* list *)

let mode_tests: (int list * int) list = [
  ([1; 2; 3; 4; 5; 5; 6; 7], 5);
  ([-1; -3; -1; -5; -1], -1);
  ([1; 2; 3; 4; 5; 6; 7; 8], 1);
  ([1; 2; 2; 3; 4; 5; 6; 6], 2);
  ([6; 5; 4; 2; 3; 1], 1); 
  ([5], 5);
  ([1; 1; 1; 1], 1); 
]
;;

let mode (l: 'a list) : 'a =
  
  let l = List.sort compare (l) in
  
  let rec aux l ((cur_el, cur_num) : 'a * int) ((max_el, max_num) : 'a * int) =
    (*notimplemented ()*)
    
    match l with
    | [] -> 
        if cur_num > max_num then
          cur_el
        else
          max_el
    | h::t -> 
        if h = cur_el then
          aux t (cur_el, cur_num+1) (max_el, max_num)
        else if
          cur_num > max_num then
          aux t (h,1) (cur_el, cur_num)
        else
          aux t (h, 1) (max_el, max_num)
          

  
          
  in
  (*notimplemented ()*)
  match l with
  | [] -> failwith "List too small." 
  | h::t -> aux t (h, 1) (h, 0)
;;

(* Question 1.2 : Most common consecutive pairing *)

let pair_mode_tests: (int list * (int * int) ) list = [
  ([5; 3; 6; 4; 8; 5; 3], (5, 3));
  ([1; 1; 1; 1; 1], (1, 1));
  
] ;;

let pair_mode (l: 'a list) : 'a * 'a = 

  match l with
  | [] -> failwith "List too small."
  | _::tl ->
      let copy_1 = tl  in
      let reversed_list = List.rev l  in
      let take_off_first =
        List.tl reversed_list  in
      let copy_2 =
        List.rev take_off_first  in
      let list_of_tuples =
        List.combine copy_2 copy_1  in
      mode list_of_tuples
  
  
;;


(* Section 2 : Custom data types *)

let convert_time ((from_unit, val_) : time_unit value) to_unit : time_unit value =

  match from_unit, to_unit with
  | Second, Hour ->  (Hour, val_ /. 3600.)
  | Hour, Second -> (Second, val_ *. 3600.)
  | Hour, Hour -> (Hour, val_)
  | Second, Second -> (Second, val_) 
;;

let convert_dist ((from_unit, val_) : dist_unit value) to_unit : dist_unit value =
  (*notimplemented ()*)
  
  match from_unit, to_unit with
  | Foot, Meter -> (Meter, val_ *. 0.3048)
  | Foot, Mile -> ( Mile, val_ /. 5280.)
  | Meter, Foot -> ( Foot, val_ /. 0.3048)
  | Meter, Mile -> ( Mile, val_ /. (0.3048 *. 5280.))
  | Mile, Foot -> ( Foot, val_ *. 5280. )
  | Mile, Meter -> ( Meter, val_ *. (0.3048 *. 5280.))
  | Foot, Foot -> (Foot, val_)
  | Meter, Meter -> (Meter, val_)
  | Mile, Mile -> (Mile, val_) 
;;

let convert_speed ((from_unit, val_) : speed_unit value) to_unit : speed_unit value =
  (*notimplemented ()*)
  
  
  let dist_1 = fst(from_unit) in
  let time_1 = snd(from_unit) in
  let dist_2 = fst(to_unit) in
  let time_2 = snd(to_unit) in
  
  let dist_converted = snd(convert_dist (dist_1, val_) dist_2) in
  let time_converted = snd(convert_time (time_1, val_) time_2) in
  
  if (dist_1, time_1) = (dist_2,time_2) then
    (dist_2,time_2), val_
  
  else
    (dist_2, time_2), val_ *. (dist_converted /. time_converted)

  

let add_speed (a : speed_unit value) ((b_unit, b_val) : speed_unit value) : speed_unit value = 
  
    
  let unit_a = fst(a) in
  let val_a = snd(a) in
    
  let conversion = convert_speed (unit_a, val_a)  b_unit in
  let new_val_a = snd(conversion) in
    
  b_unit, new_val_a +. b_val
    



let dist_traveled time ((speed_unit, speed_val) : speed_unit value) : dist_unit value = 
  
  let val_time = snd(time) in 
  let unit_time = fst(time) in 
  let time_desired = snd(speed_unit) in
  
  let new_time = convert_time (unit_time, val_time) time_desired in
  let val_new_time = snd(new_time) in
  
  let our_value = speed_val *. val_new_time in
  
  let dist_desired = fst(speed_unit) in
  
  dist_desired, our_value 
;;

(* Section 3 : recursive data types/induction *)

let passes_da_vinci_tests : (tree * bool) list = [
  (Leaf, true);
  
  (Branch (5., [Leaf]), true)
  
] ;;




let passes_da_vinci t = 
  let rec sum_ trees =(*assume you got this correct*)
    match trees with
    | [] -> 0.
    | h::t ->
        match h with
        | Leaf -> sum_ t
        | Branch (a, l1) -> a *. a +. sum_(t)
                     
  in
  
  let rec check a b c = 
    sum_(c) <= b *. b && match c with 
    | [] -> true
    | h::t -> match h with
      | Leaf -> true 
      | Branch (width, subtree) -> check (Branch(width, subtree)) width subtree
        (* return false if ((check subtree) && (sum_ subtree <= width^2)) evaluates to false *)
        (* else proceed to check tl*)
  in
  if t=Leaf then
    true
  else
    let Branch (a, d) = t in
    check t a d
    
    
    
;;
