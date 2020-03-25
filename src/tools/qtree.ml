open Printf

type rect = {
  ul : V2.t ;
  lr : V2.t
}
(* 2D RECTANGLE *)

open V2

let inside point rect =
  point.x >= rect.ul.x &&
  point.x <= rect.lr.x &&
  point.y >= rect.ul.y &&
  point.y <= rect.lr.y
(** vrai si le point est dans le rect *) 

let center rect = { x = (rect.ul.x +. rect.lr.x) /. 2. ;
		    y = (rect.ul.y +. rect.lr.y) /. 2. }


let intersect r s =
     ( r.ul.x < s.lr.x && r.lr.x > s.ul.x )
  && ( r.ul.y < s.lr.y && r.lr.y > s.ul.y )
(** [intersect r s] is [true] when the two rectangles overlap *)


type 'a t =
  | Leaf of 'a * rect
  | Node of 'a node
(* QUADTREE DEFINITION *)

and 'a node = {
  center : V2.t ;
  surf : rect ;
  nw : 'a t ;
  ne : 'a t ;
  sw : 'a t ;
  se : 'a t ;
}

let surf = function
  | Leaf (_,surf) -> surf
  | Node n -> n.surf


let ( |> ) x f = f x

let rec fold_aux pred f tree init = 
  if pred tree then 
    match tree with
      | Leaf (value, surf) -> f surf value init
      | Node n -> 
          (fold_aux pred f n.nw init)
       |> (fold_aux pred f n.sw)
       |> (fold_aux pred f n.se)
       |> (fold_aux pred f n.ne)
  else init

let default d = function 
    None -> d 
  | Some x -> x

let fold_rect ?ul ?lr f tree init =
  let zone = 
      let surf = surf tree in
      { ul = default surf.ul ul ;
	lr = default surf.lr lr } in
  let pred t = intersect zone (surf t) in
  fold_aux pred f tree init

let fold f tree init = fold_aux (fun _ -> true) f tree init

let rec map f = function
    Leaf (value, surf) -> Leaf (f surf value, surf)
  | Node n -> 
      Node { surf = n.surf ;
	     center = n.center ;
	     nw = map f n.nw ;
	     sw = map f n.sw ;
	     se = map f n.se ;
	     ne = map f n.ne }

let iter f tree = fold (fun rect value () -> f rect value) tree ()

(****************************************************************************)

type 'a set = (V2.t * 'a) list t

let empty ~ul ~lr = Leaf ([], { ul = ul ; lr = lr })

let rec insert_aux leaf_size pos value = function
  | Leaf (contents, surf) when List.length contents < leaf_size -> 
      Leaf ((pos, value) :: contents, surf)
  | Leaf (contents, surf) ->
      let c = center surf in
      let n = Node { center = c ;
		     surf = surf ;
		     nw = empty ~ul:surf.ul ~lr:c ;
		     sw = empty ~ul:{ x = surf.ul.x ; y = c.y } ~lr:{ x = c.x ; y = surf.lr.y } ;
		     se = empty ~ul:c ~lr:surf.lr ;
		     ne = empty ~ul:{ x = c.x ; y = surf.ul.y } ~lr:{ x = surf.lr.x ; y = c.y } }
      in 
      let reorganized = 
	List.fold_left
	  (fun accu (pos,value) -> insert_aux leaf_size pos value accu)
	  n contents
      in insert_aux leaf_size pos value reorganized
  | Node n ->
      let node =
	match (pos.x < n.center.x, pos.y < n.center.y) with
	    (true, true)   -> { n with nw = insert_aux leaf_size pos value n.nw }
	  | (true, false)  -> { n with sw = insert_aux leaf_size pos value n.sw }
	  | (false, false) -> { n with se = insert_aux leaf_size pos value n.se }
	  | (false, true)  -> { n with ne = insert_aux leaf_size pos value n.ne }
      in Node node

let insert leaf_size position value t =
  if inside position (surf t) then
    insert_aux leaf_size position value t
  else (
    let msg = 
      sprintf 
	"Qtree.insert: position outside bounds of qtree (%s outside of (%s,%s))" 
	(V2.to_string position)
	(V2.to_string (surf t).ul)
	(V2.to_string (surf t).lr)
    in raise (Invalid_argument msg)
  )

let set_adapter f _ contents accu = 
  List.fold_left 
    (fun accu (pos, value) -> f pos value accu)
    accu contents

let set_fold f tree init = fold_aux (fun _ -> true) (set_adapter f) tree init
(*val set_fold : (V2.t -> 'a -> 'b -> 'b) -> 'a set -> 'b -> 'b*)

let set_iter f tree = set_fold (fun v2 a () -> f v2 a) tree ()
(*val set_iter : (V2.t -> 'a -> unit) -> 'a set -> unit*)

(*let default d = function 
    None -> d 
  | Some x -> x*)


let set_fold_rect  ?(ul=None) ?(lr=None) f tree init =
  let zone = 
      let surf = surf tree in
      { ul = default surf.ul ul ;
	lr = default surf.lr lr } in
  let pred t = intersect zone (surf t) in
  fold_aux pred (set_adapter f) tree init
(*  val set_fold_rect : 
  ?ul:V2.t -> ?lr:V2.t -> 
  (V2.t -> 'a -> 'b -> 'b) -> 'a set -> 'b -> 'b*)

let set_iter_rect ?(ul=None) ?(lr=None) f tree = set_fold_rect ~ul:ul ~lr:lr (fun v2 a () -> f v2 a) tree ()
(*val set_iter_rect : ?ul:V2.t -> ?lr:V2.t -> (V2.t -> 'a -> unit) -> 'a set -> unit*)

(*EOF*)
