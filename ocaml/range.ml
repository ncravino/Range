module Range = struct 
module Make(X : sig type t 
val sub : t -> t -> t 
val add : t -> t -> t 
val eq : t -> t -> bool
val min_val : t 
val min_step : t
end) =
struct 

let to_list ?(start=X.min_val) ?(step=X.min_step) stop =
let start_val = (X.sub stop X.min_step) in
let rec range_aux curr acc =
if X.eq curr start then
curr::acc
else
let next = X.sub curr step in
range_aux next (curr::acc) 
in range_aux start_val []

let iter ?(start=X.min_val) ?(step=X.min_step) stop ~f =
let stop_val = (X.sub stop X.min_step) in
let rec range_aux curr =
if X.eq curr stop_val then
f curr
else
let next = X.add curr step in
let () = f curr in
range_aux next  
in range_aux start 

let iter_cond ?(start=X.min_val) ?(step=X.min_step) stop ~f =
let stop_val = (X.sub stop X.min_step) in
let rec range_aux curr =
let res = f curr in
match res with 
|`Continue -> 
if X.eq curr stop_val then
()
else
let next = X.add curr step in
range_aux next  
|`Repeat -> 
range_aux curr
in range_aux start 

let fold ?(start=X.min_val) ?(step=X.min_step) ~init stop ~f =
let stop_val = (X.sub stop X.min_step) in
let rec range_aux curr acc =
if X.eq curr stop_val then
f acc curr
else
let next = X.add curr step in
range_aux next (f acc curr) in
range_aux start init 
end

module Int =Make(struct 
type t = int 
let sub = fun a b -> a-b
let add = fun a b -> a + b
let eq = fun a b -> a = b
let min_val = 0
let min_step = 1
end)

module Float =Make(struct 
type t = float 
let sub = fun a b -> a-.b
let add = fun a b -> a +. b
let eq = fun a b -> a = b
let min_val = 0.0
let min_step = 1.0
end)

end
let test_list = Range.Int.to_list  6 = [0;1;2;3;4;5]
let test_fold_1 = Range.Int.fold  3 ~init: 0 ~f:(fun a x -> a +x) = 3
let test_fold_2 = Range.Int.fold  3 ~init: [] ~f:(fun a x -> x::a) = [2;1;0]
