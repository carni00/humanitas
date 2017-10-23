type t = {
  x : float ;
  y : float ;
  z : float
}

let make x y z = {
  x = x;
  y = y;
  z = z
}

let null = { x = 0. ; y = 0. ; z = 0. }
let ex = { null with x = 1. }
let ey = { null with y = 1. }
let ez = { null with z = 1. }

let map f u = {
  x = f u.x ;
  y = f u.y ;
  z = f u.z ;
}

let map2 f u v = {
  x = f u.x v.x ;
  y = f u.y v.y ;
  z = f u.z v.z ;
}

module Op = 
struct
  let ( +: ) = map2 (+.)
  let ( *: ) l = map (fun x -> l *. x)
  let ( /: ) x l = map (fun x -> x /. l) x
end

open Op

let sum uz = Array.fold_left ( +: ) null uz

let average uz = 
  let n = float (Array.length uz) 
  in (sum uz) /: n
