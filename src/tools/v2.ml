type t = { x : float ; y : float }

let null = { x = 0. ; y = 0. }
let ex = { null with x = 1. }
let ey = { null with y = 1. }
let make x y = { x = x;  y = y }

let diag x = make x x

let to_string v = Printf.sprintf "[%.2f %.2f]" v.x v.y


let map f u = {
  x = f u.x ;
  y = f u.y ;
}

let map2 f u v = {
  x = f u.x v.x ;
  y = f u.y v.y ;
}

module Op = 
struct
  let ( +: ) = map2 (+.)
  let ( -: ) = map2 (-.)
  let ( *: ) l = map (fun x -> l *. x)
  let ( /: ) x l = map (fun x -> x /. l) x
end

open Op

let center u v = (u +: v) /: 2.
let orth u = { x = -. u.y ; y = u.x }
