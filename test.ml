module ITree = struct
  type key = int * int 

  type 'a t =
      Empty
    | Node of 'a t * key * 'a * 'a t * int * int * int

  let height = function
    | Empty -> 0
    | Node(_,_,_,_,h,_,_) -> h

  let left_end = function
    | Empty -> max_int
    | Node(_,_,_,_,_,le,_) -> le

  let right_end = function
    | Empty -> min_int
    | Node(_,_,_,_,_,_,re) -> re

  let create l ((lo,hi) as x) d r =
    let hl = height l and hr = height r in
    Node(l, x, d, r, 
	 (if hl >= hr then hl + 1 else hr + 1),
	 (let le = left_end l in if lo < le then lo else le),
	 let lre = right_end l 
	 and rre = right_end r in 
	  if hi > lre then 
	    if hi > rre then hi else rre
	  else if lre > rre then lre else rre)
	   
  let bal l ((lo,hi) as x) d r =
    let hl = match l with Empty -> 0 | Node(_,_,_,_,h,_,_) -> h in
    let hr = match r with Empty -> 0 | Node(_,_,_,_,h,_,_) -> h in
    if hl > hr + 2 then begin
      match l with
          Empty -> invalid_arg "Map.bal"
	| Node(ll, lv, ld, lr, _,_,_) ->
          if height ll >= height lr then
            create ll lv ld (create lr x d r)
          else begin
            match lr with
		Empty -> invalid_arg "Map.bal"
              | Node(lrl, lrv, lrd, lrr, _,_,_)->
		create (create ll lv ld lrl) lrv lrd (create lrr x d r)
          end
    end else if hr > hl + 2 then begin
      match r with
          Empty -> invalid_arg "Map.bal"
	| Node(rl, rv, rd, rr, _,_,_) ->
          if height rr >= height rl then
            create (create l x d rl) rv rd rr
          else begin
            match rl with
		Empty -> invalid_arg "Map.bal"
              | Node(rll, rlv, rld, rlr, _,_,_) ->
		create (create l x d rll) rlv rld (create rlr rv rd rr)
          end
    end else
	Node(l, x, d, r, 
	     (if hl >= hr then hl + 1 else hr + 1),
	     (let le = left_end l in if lo < le then lo else le),
	     (let lre = right_end l 
	      and rre = right_end r in 
	      if hi > lre then 
		if hi > rre then hi else rre
	      else 
		if lre > rre then lre else rre))

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  let rec add x data = function
    | Empty ->
      Node(Empty, x, data, Empty, 1,0,0)
    | Node(l, v, d, r, h,_,_) ->
      let c = compare x v in
      if c <= 0 then
	bal (add x data l) v d r
      else
	bal l v d (add x data r)

  let rec find x = function
  Empty ->
    raise Not_found
    | Node(l, v, d, r, _,_,_) ->
      let c = compare x v in
      if c = 0 then d
      else find x (if c < 0 then l else r)

  let rec mem x = function
  Empty ->
    false
    | Node(l, v, d, r, _,_,_) ->
      let c = compare x v in
      c = 0 || mem x (if c < 0 then l else r)

  let rec min_binding = function
  Empty -> raise Not_found
    | Node(Empty, x, d, r, _,_,_) -> (x, d)
    | Node(l, x, d, r, _,_,_) -> min_binding l

  let rec remove_min_binding = function
  Empty -> invalid_arg "Map.remove_min_elt"
    | Node(Empty, x, d, r, _,_,_) -> r
    | Node(l, x, d, r, _,_,_) -> bal (remove_min_binding l) x d r

  let merge t1 t2 =
    match (t1, t2) with
	(Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
	let (x, d) = min_binding t2 in
	bal t1 x d (remove_min_binding t2)

  let rec remove x = function
  Empty ->
    Empty
    | Node(l, v, d, r, h,_,_) ->
      let c = compare x v in
      if c = 0 then
	merge l r
      else if c < 0 then
	bal (remove x l) v d r
      else
	bal l v d (remove x r)

  type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration

  let rec cons_enum m e =
    match m with
	Empty -> e
      | Node(l, v, d, r, _,_,_) -> cons_enum l (More(v, d, r, e))

end

module Gen = struct
  open Batteries

  let random_interval ?(lb = 0) ?(ub = 100) ?(minw = 1) ?(maxw = 30) _ = 
    assert (maxw < ub - lb) ;
    let w = Random.int (maxw - minw) + minw in
    let lo = Random.int (ub - lb - w) + lb in
    (lo, lo + w, ())
      
  let random_intervals ?(lb = 0) ?(ub = 100) ?(minw = 1) ?(maxw = 30) n = 
    (1 -- n) /@ (random_interval ~lb ~ub ~minw ~maxw)
end

let test_intervals f = 
  for i = 1 to 100 do 
    ignore (f (BatList.of_enum (Gen.random_intervals ~ub:1000 1000)))
  done

module M = Map.Make(struct type t = int * int let compare = compare end)

let test_map intervals = 
  List.fold_left
    (fun accu (lo,hi,v) -> M.add (lo,hi) v accu)
    M.empty intervals

let test_itree intervals = 
  List.fold_left
    (fun accu (lo,hi,v) -> ITree.add (lo,hi) v accu)
    ITree.empty intervals

let test_itree2 intervals = 
  List.fold_left
    (fun accu (lo,hi,v) -> Biocaml_intervalTree.add lo hi v accu)
    Biocaml_intervalTree.empty intervals

let _ = Bench.bench [
  ("map",    fun () -> test_intervals test_map);
  ("itree",  fun () -> test_intervals test_itree);
  ("itree2", fun () -> test_intervals test_itree2); 
]
