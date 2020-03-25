(*

 ****************************** color.ml ******************************


 *  This file is part of Humanitas.

 *  Humanitas is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License,
 *  or (at your option) any later version.

 *  Humanitas is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.

 *  You should have received a copy of the GNU General Public License
 *  along with Humanitas; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

 *)
open Std

type rvb = RVB of (int*int*int)

let iMax = 1000 (*intensité maximum*)
let lMax = 1000 (*luminosité maximum*)
let cMax = 1000 (* valeur maximum du r, v, ou b en rvb *)

(********************************* Module Nuance ***************************************)
module Nuance = struct

  open Tfloat
  type t = float
  let (%) = Tfloat.modulo
  let degres = 20 (*nuances majeures = nommées (constante arbitraire)*)
  let arc    = 1.   (*espace séparant deux nuances majeurs (constante arbitraire)*)
  let circ   = foi degres * arc (*circonférence du cercle chromatique*)
  let legal  n = (n%circ)
  let custom n = legal n
  let add n nl = legal (n+nl)
  let cut nMin nMax n = min (max (legal n) nMin) nMax
  

  let array = [|
     0.00, RVB (   0,    0, 1000),  500,   990, "bleu";
     0.50, RVB (   0,  320, 1000),  600,   900, "";
     1.00, RVB (   0,  580, 1000),  710,   800, "cyan";
     1.50, RVB (   0,  720, 1000),  768,   700, "";
     2.00, RVB (   0,  840, 1000),  825,   600, "ocean";
     2.50, RVB (   0,  920, 1000),  862,   500, "";
     3.00, RVB (   0, 1000, 1000),  900,   400, "turquoise";
     3.50, RVB (   0, 1000,  950),  882,   300, "";
     4.00, RVB (   0, 1000,  915),  865,   200, "celadon";
     4.50, RVB (   0, 1000,  860),  847,   300, "";
     5.00, RVB (   0, 1000,  760),  830,   400, "amande";
     5.50, RVB (   0, 1000,  645),  805,   500, "";
     6.00, RVB (   0, 1000,  485),  780,   600, "pomme";
     6.50, RVB (   0, 1000,  320),  730,   800, "";
     7.00, RVB (   0, 1000,    0),  680,   990, "vert";
     7.50, RVB ( 280, 1000,    0),  685,   900, "";
     8.00, RVB ( 540, 1000,    0),  690,   800, "lime";
     8.50, RVB ( 770, 1000,    0),  695,   700, "";
     9.00, RVB (1000, 1000,    0),  700,   600, "jaune";
     9.50, RVB (1000,  915,    0),  695,   600, "";
    10.00, RVB (1000,  830,    0),  690,   600, "ble";
    10.50, RVB (1000,  758,    0),  682,   700, "";
    11.00, RVB (1000,  685,    0),  675,   800, "ambre";
    11.50, RVB (1000,  602,    0),  648,   900, "";
    12.00, RVB (1000,  520,    0),  620,   990, "orange";
    12.50, RVB (1000,  400,    0),  565,   950, "";
    13.00, RVB (1000,  280,    0),  510,   900, "corail";
    13.50, RVB (1000,  140,    0),  462,   950, "";
    14.00, RVB (1000,    0,    0),  415,   990, "rouge";
    14.50, RVB (1000,    0,  110),  432,   950, "";
    15.00, RVB (1000,    0,  220),  450,   900, "cerise";
    15.50, RVB (1000,    0,  300),  462,   900, "";
    16.00, RVB (1000,    0,  380),  475,   900, "magenta";
    16.50, RVB (1000,    0,  510),  508,   940, "";
    17.00, RVB (1000,    0,  640),  540,   990, "violine";
    17.50, RVB (1000,    0,  820),  590,   990, "";
    18.00, RVB (1000,    0, 1000),  640,   990, "violet";
    18.50, RVB ( 820,    0, 1000),  605,   990, "";
    19.00, RVB ( 640,    0, 1000),  570,   990, "indigo";
    19.50, RVB ( 440,    0, 1000),  535,   990, "";
     0.00, RVB (   0,    0, 1000),  500,   990, "bleu";
      |]
  (* décomposition en RVB des couleurs repères + luminosité de chacun de ces RVB + intensité + nom. *)
  (* le premier élément doit être répété en dernier élément *)
  
  let rvb   i = snd5(array.(i))
  let lum   i = trd5(array.(i))
  let inten i = frh5(array.(i))
  
  let minArc  = 0.5
  (* arc mineur : nombre de degrés séparant chaque ligne de *array *)
  let line nuance = iof (nuance/minArc)
  
  let mean a b i =
    let i = cut 0. minArc i in
    iof (((foi a)*(minArc-i) + (foi b)*i)/minArc)
  (* moyenne entre les éléments a et b de deux lignes de *array *)
  
  let to_rvb n = 
    let rvbMean (RVB(r1,v1,b1)) (RVB(r2,v2,b2)) i = RVB(mean r1 r2 i, mean v1 v2 i, mean b1 b2 i) in
    let i = line n in
    rvbMean (rvb i) (rvb (i ++ 1)) (n % minArc)
  (* associe à une nuance n un RVB « pur » *)

  let lumina n = 
    let i = line n in
    mean (lum i) (lum (i ++ 1)) (n % minArc)
  (* associe à une nuance n la luminosité naturelle de cette nuance*)

  let intens n = 
    let i = line n in
    mean (inten i) (inten (i ++ 1)) (n % minArc)
  (* associe à une nuance n l’intensité naturelle de cette nuance*)

  (*********************** nuances nommées ****************************)
  let degre       i = fst5(array.(i))
  let name_of_int i = ffh5(array.(i))
  let name_strn   n = name_of_int(line n)
  let float_strn    = Strn.float (-2)
  let to_strn     n = if floor n=n then name_strn n else float_strn n
  let none          = 0.

  let g strn =
    let length = Array.length array in
    let rec f = function
    | i when i=length             -> none
    | i when name_of_int i = strn -> degre i
    | i -> f (i++1) in f 0
  
  let bleu      = g "bleu"
  let cyan      = g "cyan"
  let ocean     = g "ocean"
  let turquoise = g "turquoise"
  let celadon   = g "celadon"
  let amande    = g "amande"
  let pomme     = g "pomme"
  let vert      = g "vert"
  let lime      = g "lime"
  let jaune     = g "jaune"
  let ble       = g "ble"
  let ambre     = g "ambre"
  let orange    = g "orange"
  let corail    = g "corail"
  let rouge     = g "rouge"
  let cerise    = g "cerise"
  let magenta   = g "magenta"
  let violine   = g "violine"
  let violet    = g "violet"
  let indigo    = g "indigo"


  let arithmean a b =
    let a,b = legal a, legal b in 
    let a,b = if a<b then a,b else a,b+circ in
    legal ((a + b) / 2.)

  let weighmean a b x y = 
    let a,b = legal a, legal b in 
    let a,b = if a<b then a,b else a,b+circ in
    legal (a + (b - a) * (y / (x+y)))

  end

module N = Nuance
(********************************* Module Nil ***************************************)
module Nil = struct

  type t = N.t*int*int
  let legal (n,i,l) = (N.legal n), (cut 0 iMax i), (cut 0 lMax l)
  let make   n i l  = legal (n,i,l)
  let nuance = fun (n,_,_) -> n
  let intens = fun (_,i,_) -> i
  let lumina = fun (_,_,l) -> l
  let nuaadd (n,i,l) nd = (N.add n nd,i,l)
  let intadd (n,i,l) id = (n,i+id,l)
  let lumadd (n,i,l) ld = (n,i,l+ld)
  (* let composition_strn (n,i,l) = 
   *   Printf.sprintf "{n=%d;i=%d;l=%d}" (n:>int) i l *)
  let arithmean (n,i,l) (o,j,m) = (N.arithmean n o, Ext.arithmean i j, Ext.arithmean l m)

 
end
(**************************************************************************************)

module Rvb = struct

  type t = rvb
  let legal c = cut 0 cMax c
  let make r v b = RVB(legal r, legal v, legal b)

  let add (RVB(r,v,b)) (RVB(s,w,c)) = 
    let f i j = min (i+j) cMax in
    RVB((f r s), (f v w), (f b c))
  (* synthèse additive de 2 rvb *)

  let lumina (RVB(r,v,b)) =
    let rlu = Nuance.lumina N.rouge
    and vlu = Nuance.lumina N.vert
    and blu = Nuance.lumina N.bleu in
    (r*rlu+ v*vlu + b*blu) / (rlu + vlu + blu)
  (* luminosité d’un RVB *)

 
  let intensite = fun (RVB(r,v,b)) realiMax lum i ->
    let iMod = if i >= realiMax then iMax else Ext.rangeMean 0 iMax realiMax i in
    let f c = Ext.rangeMean (lum*cMax/lMax) c iMax iMod in 
    RVB(f r, f v, f b),lum
  (* transforme un RVB pur (ie d’intensité maximum == intens) en un RVB d’intensité i
     et de même luminosité (lum) (approximativement en réalité) *) 
  (* moyenne (pondérée par l’intensité i) entre chacunes des composants r v b du rvb donnée et celles du gris de
  luminosité équivalente (dont les composantes r v b valent lum,lum,lum *)
  (** [rangeMean a b range d] returns [(a*(range-d) + b*d)/range] *) 
  
  let luminosite = fun ((RVB(r,v,b)),lum) l ->
    let f c = if (l-lum) > 0 
              then Ext.rangeMean c cMax (lMax-lum) (l-lum)
              else Ext.rangeMean 0 c     lum        l  in
    RVB(f r, f v, f b)
  (* modifie la luminosité d’un RVB de lum, en l *) 

  let lum_mult = fun (RVB(r,v,b)) i -> 
    let f c = min lMax (iof (foi c *. i)) in
    RVB (f r, f v, f b)
  (* ajuste la luminosité d'un RVB d'un facteur r *)
  

  let of_nil = fun ((n,i,l)) ->
    let n,i,l = Nil.legal (n,i,l) in luminosite (intensite(N.to_rvb n) (N.intens n) (N.lumina n) i) l
  (* associe à un NIL un RVB *)

  let black  = RVB(   0,   0,   0)
  let red    = RVB(cMax,   0,   0)
  let yellow = RVB(cMax,cMax,   0)
  let green  = RVB(   0,cMax,   0)
  let cyan   = RVB(   0,cMax,cMax)
  let blue   = RVB(   0,   0,cMax)
  let magenta= RVB(cMax,   0,cMax)
  let white  = RVB(cMax,cMax,cMax)
  let gray x = RVB(   x,   x,   x)

  let to_255 x = x*255/cMax

  let to_irr alpha (RVB(r,v,b)) = 
    let f = to_255 in
    let a,r,v,b = iof(alpha *. 255.), f r, f v, f b in
    (a, r, v, b)
  (* Std.Irr_core.color_ARGB ~a ~r ~v ~b *)
  
  let to_sdlvideo = fun (RVB(r,v,b)) ->
    let f = to_255 in
    let r,v,b = f r, f v, f b in
    Int32.of_int((r lsl 16) + (v lsl 8) + b)
  (* associe à un RVB un int32 lisible par Sdlvideo*)
  
  let to_sdlgfx  ?(a=1.0)  (RVB(r,v,b)) =
    let f = to_255 in
    let r,v,b = f r, f v, f b in
    Int32.of_int((r lsl 24) + (v lsl 16) + (b lsl 8) + iof(a *. 255.) )
  (* associe à un RVB un int32 lisible par Sdlgfx*)
  
  let to_sdlttf t = match t with RVB(r,v,b) -> 
    let f = to_255 in f r, f v, f b
  (* associe à un RVB un triplet (r,v,b) lisible par Sdlttf *)

  (* let composition_strn (RVB(r,v,b)) = 
   *   Printf.sprintf "{r=%d;v=%d;b=%d}" r v b *)

  let of_sdlvideo svco =
    let i = Int32.to_int svco in
    let f i d = ((i lsr d) mod 256) * cMax / 255 in
    make (f i 16) (f i 8) (f i 0)



end

(***************** Module Color stricto sensu ****************)

type t     = 
| RGB of Rvb.t
| NIL of Nil.t

let of_nil nil = NIL nil
let of_rvb rvb = RGB rvb
(*let of_int int = of_rvb (Rvb.of_int int)*)
(*let to_int co  = match co with
| RGB rvb -> Rvb.to_int rvb
| NIL nil -> raise (Failure "Color.to_int")*)

let nil n i l = of_nil (Nil.make n i l)
let rvb r v b = of_rvb (Rvb.make r v b)

let of_nuance n = nil n iMax (N.lumina n)
  
let bleu      = of_nuance N.bleu
let cyan      = of_nuance N.cyan
let ocean     = of_nuance N.ocean
let turquoise = of_nuance N.turquoise
let celadon   = of_nuance N.celadon
let amande    = of_nuance N.amande
let pomme     = of_nuance N.pomme
let vert      = of_nuance N.vert
let lime      = of_nuance N.lime
let jaune     = of_nuance N.jaune
let ble       = of_nuance N.ble
let ambre     = of_nuance N.ambre
let orange    = of_nuance N.orange
let corail    = of_nuance N.corail
let rouge     = of_nuance N.rouge
let cerise    = of_nuance N.cerise
let magenta   = of_nuance N.magenta
let violine   = of_nuance N.violine
let violet    = of_nuance N.violet
let indigo    = of_nuance N.indigo


let black     = of_rvb Rvb.black
let white     = of_rvb Rvb.white
let gray                         l = of_rvb (Rvb.gray l)
let brown ?(n=N.orange) ?(i=425) l = nil n i l

let red       = rouge
let yellow    = jaune
let green     = vert
let blue      = bleu

let lumina = function
| RGB rvb -> Rvb.lumina rvb
| NIL nil -> Nil.lumina nil

let lumadd co int = match co with
| NIL nil -> NIL(Nil.lumadd nil int)
| _ -> raise (Failure "Color.lumadd ne gere pas les RVB")

let arithmean co cp = match co, cp with
| NIL nil, NIL ojm -> of_nil (Nil.arithmean nil ojm)
| _ -> raise (Failure "Color.arithmean ne gere pas les RVB")

let coFun_of_rvbFun rvbFun = function 
| RGB rvb -> rvbFun rvb
| NIL nil -> rvbFun (Rvb.of_nil nil)

let to_irr alpha = coFun_of_rvbFun (Rvb.to_irr alpha)
let to_sdlvideo  = coFun_of_rvbFun (Rvb.to_sdlvideo)
let to_sdlgfx ?a = coFun_of_rvbFun (Rvb.to_sdlgfx ?a)
let to_sdlttf    = coFun_of_rvbFun (Rvb.to_sdlttf  )


let of_sdlvideo  = compose of_rvb (Rvb.of_sdlvideo)

(*let composition_strn = function
| RGB rvb -> Rvb.composition_strn rvb
| NIL nil -> Nil.composition_strn nil*)

(*EOF*)
