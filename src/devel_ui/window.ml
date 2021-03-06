(*

 ****************************** window.ml ******************************


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

open Humanitas_tools
open Humanitas_physis
open Humanitas_orbis
open Humanitas_game
open Std
module E  = Espace
module Rvi= Rv.Incola
module Co = Color
module N  = Natio
module K  = Sdlkey
module W  = WindowID
module SA = Status.Atelier
module SS = Scene
module RS = React.S
module Nil= Nid.Nil 
module Gn = G.Natio
module P  = Partitio
module Luc= Lucrum

type title = string React.signal
type button = Sdlkey.t * string * Task.t list

type agencement = 
| Columns 
| Lines of (Frui.valign Frui.spacing * Frui.halign)

type element =
| Rect of (float * float * Color.t)
| LB of button
| SB of button
| S  of string
(*| Label of (float * float * string)*)
| Z  of string React.signal
| Box of (float * float * Frui.halign * element)
| List of (agencement * element list)

type contents = title * element
type id       = WindowID.t

let rsm  = RS.map
(*let rsm2 = RS.l2*)
let c    = RS.const
let juce   = (`justified,`center)
let _juri   = (`justified,`right)
let _jule   = (`justified,`left)
let toce   = (`justified,`center)
let _tori   = (`justified,`right)
let tole   = (`justified,`left)

(********************************* Orbis windows **********************************)
let newspaper atelier =
  let pov  = Game.Player.pov (SA.player atelier) in
  let game = Status.Atelier.game atelier in
  let o    = Game.orbis game in
  let line evt = Box (16., 1., `left, S (Si.eventum evt) ) in
  c( Si.newspaper pov ), List(Lines juce,
  Box (16., 1., `left, S " ") ::
  List.map line (List.rev o.Orbis.addendum ) @ [
  LB( K.KEY_o       , "OK"                            , [`wClose W.Newspaper] );
  LB( K.KEY_F9      , "F9  : open the archives"       , [`wOpen (W.Vetera, W.Default); `sFocus (Some W.Central) ] );
  LB( K.KEY_F12     , "F12 : go to next event"        , [`wClose W.Newspaper; `next_event ] );
  ] )
let vetera atelier =
  let game = Status.Atelier.game atelier in
  let o    = Game.orbis game in
  let line evt = Box (16., 1., `left, S (Si.eventum evt) ) in
  c("   Vetera   "), List(Lines juce,
  Box (16., 1., `left, S " ") ::
  List.map line (List.rev (Tlist.n_first o.Orbis.vetera 20)) ) (*20 derniers evenements*)


let tabula atelier =
  let scale = Strn.percent (0) (Scene.ascale (Status.Atelier.scene atelier)) in
  let wik   = Strn.float   (2) (Scene.wik    (Status.Atelier.scene atelier)) in
  c "Tabula", List(Lines juce, [
  S( "zoom level    : "^scale );
  S( "scene width   : "^wik^" km" );
    S "---"  ;
    S "Map shortcuts"  ;
  LB( K.KEY_t       , "T : open this window"    , [`wOpen (W.Tabula, W.Default)           ] );
  LB( K.KEY_r       , "shift + left button : Regio info" , [`secure_sr ; `wOpen (W.Regio, W.Default)     ] );
  LB( K.KEY_z       , "Z or + : Zoom in"             , [`zoom_in                                ] );
  LB( K.KEY_w       , "W or - : zoom out"            , [`zoom_out                               ] );
  LB( K.KEY_e       , "E : toggle Earth mode"       , [`toggle_earthMode                       ] );
  LB( K.KEY_b       , "B : toggle Borders"          , [`toggle_borders                         ] );
  LB( K.KEY_a       , "A : toggle Altitude"         , [`toggle_altitude                        ] );
  LB( K.KEY_n       , "N : toggle Nations"          , [`toggle_nations                         ] );
  LB( K.KEY_HOME    , "Home : move to Homeland"        , [`move_to_capitolium                     ] );
  LB( K.KEY_g       , "G : toggle backGround"       , [`switch_background                      ] );
  LB( K.KEY_f       , "F : tabula Filters"          , [`wOpen (W.Filters, W.Default)             ] );
(*  B( K.KEY_g       , "display/hide map Grid"   , [`                                       ] );*)
(*  B( K.KEY_p       , "display/hide Polyhedron" , [`                                       ] );*)
(*  LB( K.KEY_h       , "hide Stacks"             , [`sHide W.Left; `sHide W.Right           ] );*)
(*  LB( K.KEY_y       , "restore default displaY" , [`defaultDisplay                         ] );*)
  ])

  
let orbis atelier =
  let game = Status.Atelier.game atelier in
  let o    = Game.orbis game in
  let nl   = o.Orbis.natioList in
  let sof i f = Strn.float i (f nl) in
  let box  a strn = Box (6.6, 1., a, S strn) in
  let line strn1 strn2 = List (Columns, [box `right strn1 ; S "  :  " ; box `left strn2] ) in
  let line_of_cvt cvt = line ( Si.civitas cvt ) ( "toto" ) in
    c("Orbis"), List(Lines toce, [
    line "plebs"  (Si.plebs (NatioList.plebs nl)) ;
    line "instrumentum"   (sof   0  NatioList.instrumentum   ) ;
    line "fides"          (sof (-2) NatioList.fides   ) ;
    line "sophia"         (sof (-2) NatioList.sophia   ) ;
    ] @ CivitasList.map line_of_cvt (CivitasList.rev o.Orbis.civitasList)  )

let atelier a wid = 
  let f = function 
| W.Tabula  -> tabula
| W.Vetera  -> vetera
| W.Newspaper-> newspaper
| _         -> orbis in  
  (f wid) a

(********************************* Natio windows **********************************)



let fines atelier nid =
  let game = Status.Atelier.game atelier in
  let o    = Game.orbis game in
  let luc  = o.Orbis.lucrum in
  let n    = NatioList.get o.Orbis.natioList nid in
  let g    = Natio.geographia n in
  let sof  = Strn.float in

  let box w strn = Box (foi w,  1., `right, S strn) in
  let void       = Box (   1.,  1., `left,  S " | " ) in
  let lin5 a b c d e f = List (Columns, box 4 a :: List.fold_left (fun a b -> void :: box 4 b :: a) [] [f;e;d;c;b]) in
  let line t amp ple p = lin5 t (sof 3 amp) (sof 4 ple) (sof 0 (P.lab p)) (sof 0 (P.mil p)) (sof 0 (P.lux p)) in
  let linc t c       p = line t  c.Rv.superficies c.Rv.plebs  p          in (*line chora*)
  let linf t amp ple p = line t  (amp g)          (ple g)    (p luc nid) in

  let damnList= Nil.mapi_to_list (fun i c -> linc ("("^Si.natioAdj i^" imperium)") c (Luc.damnum   luc nid i)) (Gn.funusList g) in
  let tribList= Nil.mapi_to_list (fun i c -> linc ("("^Si.natioAdj i^" chora   )") c (Luc.tributum luc nid i)) (Gn.finesList g) in

  c("   "^Si.natioName nid^" - Fines   "), List( Lines tole, [
    lin5 " "           "Amplitudo (km2)"   "Plebs"      "Cibus"     "Militaria" "Luxus" ;
    lin5 " ------------ "     "-------- "   "-------- " "-------- " "-------- " "-------- " ;
    linf " Chora "          Gn.choraAmp     Gn.plebs     Luc.factum  ;
    lin5 " ------------ "     "-------- "   "-------- " "-------- " "-------- " "-------- " ;
    ] ^^ damnList ^^ [
    linf "Damnum sum."      Gn.funuSumAmp   Gn.funuSumPle Luc.damSum             ;
    lin5 " ------------ "     "-------- "   "-------- " "-------- " "-------- " "-------- " ;
    ] ^^ tribList ^^ [
    linf "Tributum sum."    Gn.fineSumAmp   Gn.fineSumPle Luc.tribuSum              ;
    lin5 " ------------ "     "-------- "   "-------- " "-------- " "-------- " "-------- " ;
    linf " Imperium "       Gn.impAmp       Gn.impPle    Luc.lucrum                   ;
    ] )


let chora atelier nid =
  let game = Status.Atelier.game atelier in
  let o    = Game.orbis game in
  let n    = NatioList.get o.Orbis.natioList nid in
  let g    = Natio.geographia n in
  let sof  = Strn.float in

  let box strn = Box (4.,  1., `right, S strn) in
  let void     = Box (1.,  1., `left,  S " | " ) in
  let lin6 a b c d e f = List (Columns, List.fold_left (fun a b -> box b :: void :: a) [box f] [e;d;c;b;a]) in
  let linc a c = lin6 a (sof   3  c.Rv.superficies) 
                        (sof ( 5) c.Rv.facultas)
                        (sof ( 0) c.Rv.hospitalitas)
                        (sof ( 0) c.Rv.instrumentum)
                        (sof ( 4) c.Rv.plebs) in
  let funusList    = Nid.Nil.mapi_to_list (fun i c -> linc ("("^Si.natioAdj i^" imperium)") c)   (G.Natio.funusList g) in
  c("   "^Si.natioName nid^" - Chora   "), List( Lines tole, [
    lin6 " "                        "Superficie (km2)"   "Facultas" "Hospitalitas" "Instrumentum" "Plebs" ;
    lin6 (Si.natioAdj nid^" chora") " "                  " "        " "            " "            " "     ;
    linc "(under control)"          (G.Natio.centrum g)
    ] ^^ funusList ^^ [
    linc "(total)"                  (G.Natio.chora g);
    ] )

let tactics atelier nid =
  let game = Status.Atelier.game atelier in
  let o    = Game.orbis game in
  let s    = StrategicaList.get_strategica (Game.strategies game) nid in
  let box  a strn = Box (6.6, 1., a, S strn) in
  let line strn1 strn2 = List (Columns, [box `right strn1 ; S "  :  " ; box `left strn2] ) in
  let nil      = Nid.none :: (Nid.List.sort (Natio.pil (NatioList.get o.Orbis.natioList nid))) in
  let imp  nid = if nid = Nid.none then "unclaimed territory" else Si.natioName nid in
  let line nid = line (imp nid) (Si.tactic (Strategica.tactic s nid)) in
    c("   "^Si.natio Si.Name nid^" - Last tactics   "), 
    List(Lines toce, box  `center "Last tactic towards : "  :: (Nid.Nil.to_list (Nid.Nil.init line nil))
(*    line "unclaimed territory"  (Si.tactic (Strategica.tactic s Nid.none)) ; ]*)
    )

let artes atelier nid =
  let game = Status.Atelier.game atelier in
  let o    = Game.orbis game in
  let n    = NatioList.get o.Orbis.natioList nid in
  let artes= N.artes n in
  c("   "^Si.natio Si.Name nid^" - Artes   "),
  List(Lines toce, List.map (fun ars -> S (Si.ars ars)) artes)


let partitio atelier nid =
  let game = Status.Atelier.game atelier in
  let o    = Game.orbis game in
  let n    = NatioList.get o.Orbis.natioList nid in
  let module P = Partitio in
  let module F = Partitio.Record in
  let partitio = Natio.partitio n in
  let record   = F.compute (partitio) (Natio.pNatio n) in
  let box  a strn = Box (4., 1., a, S strn) in
  let perc g f = Strn.percent (-2) (f (g record) ) in 
  let line  attrib value actio fructus factum = List (Columns, [
                                                box `right attrib ; S "  :  " ; 
                                                box `right value  ; S "  :  " ; 
                                                box `right actio  ; S "  :  " ; 
                                                box `right fructus; S "  :  " ; 
                                                box `right factum ] )in
  let lineb s f = line  s (perc F.attrib f) (perc F.actio f) (perc F.fructus f) (Strn.float (3) (f (F.factum record) )) in
  let linec a   = lineb (Si.attributio a) (fun p -> P.attrib p a) in
    c(Si.natio Si.Name nid ^" - Partitio"), List(Lines toce, [
    line "-" "attrib" "actio" "fructus" "factum";
    linec P.LAB;
    linec P.SAP;
    linec P.REL;
    linec P.MIL;
    linec P.OPP;
    linec P.LUX;
    linec P.OTI;
    line "-----" "-----" "-----" "-----" "-----";
    lineb "summa" P.summa
    ])
 let last_artes int artes = 
  let rec f found left = match found, left with
  | _    , []   -> found 
  | found, _    when Tlist.len found = int -> found 
  | found, e::q -> f (e::found) q in
  let list = f [] (List.rev artes) in
  match list with [] -> failwith "Window.last_artes" | first::q ->
  List.fold_left (fun s e -> s^", "^(Si.arsID e)) (Si.arsID first) q


let polis atelier nid =
  let game = Status.Atelier.game atelier in
  let o    = Game.orbis game in
  let n    = NatioList.get o.Orbis.natioList nid in
  let sof i f = Strn.float i (f n) in
  let box     a strn = Box (5., 1., a, S strn) in
  let line  strn1 strn2         = List (Columns, [box `right strn1 ; S "  :  " ; box `left strn2 ] ) in
  let gate  strn        key wid = LB( key, strn, [`wOpen ( wid, W.Default)] ) in
  let lineB strn1 strn2 key wid = gate (strn1^"  :  "^strn2) key wid in
    c(Si.natio Si.Name nid), List(Lines toce, [
    line  "politeia"    (Politeia.to_string  (N.politeia n)) ;
    line  "Vis"         (sof (-2) N.vis   ) ;
    lineB "Origo"       (Si.date (snd(N.origo n)))    K.KEY_o  W.Vetera  ;
    lineB "Artes"       (last_artes 2 (N.artes n))     K.KEY_a  W.Artes   ;
    lineB "Chora"       (sof   4  N.chora    ^" km2")  K.KEY_c  W.Chora   ;
    lineB "Imperium"    (sof   4  N.imperium ^" km2")  K.KEY_i  W.Fines   ;
    lineB "Facultas"    (sof   5  N.facultas   )       K.KEY_f  W.Dx ;
    lineB "plEbs"       (Si.plebs (N.plebs n))         K.KEY_e  W.Pyramid ;
    gate  "Partitio"                                   K.KEY_p  W.Partitio;
    line "populatio"    (Si.population(N.plebs n)((N.chora n)))  ;
    line "occupatio"    (Strn.percent (-1) (1. /. N.agriCopia n))  ;
(*    line "seditiones"   N.seditiones    ;*)
    line "hospitalitas" (sof   0  N.hospitalitas   ) ;
    line "instrumentum" (sof   0  N.instrumentum   ) ;
    line "efficientia"  (sof (-2) N.efficientia   ) ;
    line "sophia"       (sof (-2) N.sophia   ) ;
    line "fides"        (sof (-2) N.fides   ) ;
    line "latifundium"  (sof (-2) N.latifundium   ) ;
    ])

let dx atelier nid =
  let game = Status.Atelier.game atelier in
  let o    = Game.orbis game in
  let n    = NatioList.get o.Orbis.natioList nid in
  let sof i f = Strn.float i (f n) in
  let box  a strn = Box (5., 1., a, S strn) in
  let line  strn1 strn2   = List (Columns, [box `right strn1 ; S "  :  " ; box `left strn2 ] ) in
    c("    "^Si.natio Si.Name nid ^ " - Demographics    "), List(Lines toce, [
    line "facultas"     (sof   5  N.facultas   ) ;
    line "alimonium_R"  (sof (-3) N.alimonium_ratio )  ;
    line "fac. * alim_R" (sof  5  (fun n -> N.facultas n /. N.alimonium_ratio n) )  ;
    line "plebs"        (Si.plebs (N.plebs n))   ;
    line "dxVar"        (sof (-3) N.dxVar )  ;
    line "ss-nutr. - mortal." (sof (-3) N.famine )  ;
(*    line "ss-nutr. - mortal." (Strn.percent (-2) (N.famine n))  ;*)
    line "dxCopia"      (Strn.percent (-1) (N.copia n))  ;
    line "tfg"          (Strn.percent (-1) (N.tfg n))  ;
    line "ind. fecond." (sof (-2) N.isf ) ;
    line "desc. finale" (sof (-2) N.dfn   )  ;
    ])



let pyramid atelier nid =
  let game = Status.Atelier.game atelier in
  let o    = Game.orbis game in
  let n    = NatioList.get o.Orbis.natioList nid in
  let pyramid = Dx.Pyramid.vigesimal (Natio.pyramid n) in
  let nbList  = List.map (fun (_tranche, nb) -> nb) pyramid in
  let numerousGen = Tlist.max nbList in
  let line i = 
    let tranche = let min=i*5 in let max=min+4 in [min;max] in
    let nb = List.assoc tranche pyramid in
    let rect = Rect ((12. *. nb /. numerousGen), 1.0, Co.blue)  in
    let strn = match tranche with [a;b] -> ((soi a)^".."^(soi b)) | _ -> raise (Failure "Win.pyramid") in
    let box  = Box (2.3, 0.6, `center, S strn) in
    List(Columns, [rect; box ;  rect ]) in
  c(Si.natio Si.Name nid ^ " - Pyramid"), List(Lines toce, List.rev (Tlist.init 20 line) )


let natio atelier nid wid = 
  let f = function 
| W.Artes   -> artes
| W.Chora   -> chora
| W.Dx      -> dx
| W.Fines   -> fines
| W.Partitio-> partitio
| W.Pyramid -> pyramid
| W.Tactics -> tactics
| _         -> polis in  
  (f wid) atelier nid
  
(********************************* Regio window **********************************)

let regio atelier rid =
  let game    = Status.Atelier.game atelier in
  let o       = Game.orbis game in
  let e       = Orbis.espace o in
  let rm      = Orbis.regioMap o in
  let im      = o.Orbis.imperiumMap in
  let r       = Rm.get rm rid in
  let rv      = Im.get im rid in
  let lat,lon = Espace.Regio.coords e rid in
  let rs,rw,rh= Espace.rdim ~lat e in
  let tegmen  = Rv.tegmen ~rv r in
(*  let is_climax = (match tegmen with Rv.Desertum n -> (n = R.climax r) | _ -> false ) in*)
  let box  a strn = Box (6.6, 1., a, S strn) in
  let line strn1 strn2 = List (Columns, [box `right strn1 ; S "  :  " ; box `left strn2] ) in
  let empty_line = line " " " " in
  let physis_lines = [
    line "dimensions" (Strn.float 0 rw^" x "^Strn.float 0 rh^" (km)") ;
    line "altitude" (Si.altitude(R.altitude r)) ;
    line "climate"  (Si.climat(R.climat r))     ;
    line "temperature"                  ((soi(R.thermos r))^"\176C") ;
    line "precipitations"               ((soi(R.pluvia r))^" mm")      ;
    line (Si.river (R.fluxus r))        (Si.debit(R.fluxus r))        ;
    line "physis"                       (soi(R.physis r ))       ;
    line "hospitalitas"                 (Strn.float   0  (R.hospitalitas r))       ;
    ]
  and incola_lines = match Rv.contents rv with
  | Rv.Desertum_for _n -> [
    empty_line;
    empty_line;
    line "tegmen"                       (Si.tegmen tegmen) ;
    empty_line;
    empty_line;
    line "dominus"                      (Si.natioName(Rv.dominus rv))    ;
    empty_line;
    ]
  | Rv.Incol inc      -> [
    line "instrumentum"                 (Strn.float (-0) (Rvi.instrumentum inc))       ;
    line "( climax"                     (Si.climax(R.climax r)^" )")  ;
    line "tegmen"                       (Si.tegmen tegmen) ;
    line "facultas"                     (Strn.float 3 (Rv.facultas r rv))  ;
    line "plebs"                        (Si.plebs     (Rvi.plebs inc)   )   ;
    line "population"                   (Si.population(Rvi.plebs inc) rs)   ;
    line "incola"                       (Si.natioName(Rvi.nid inc))     ;
    line "dominus"                      (Si.natioName(Rv.dominus rv))    ;
    line "dominium"                     (Si.dominium (Rvi.dominium inc))    ;
    ]
  and civitas_lines = match CivitasList.search o.Orbis.civitasList rid with 
  | Some civitas -> [
    line (Si.civitas civitas) " " ;
    line "founded in"                   (Si.date (Civitas.origo civitas) ) ;
    line "urbani"                       (Si.plebs (Civitas.plebs civitas) ) ;
  
  
  ]
  | _  -> [] in

  c("Regio : "^Strn.coords lat lon), List(Lines toce, physis_lines ^^ incola_lines ^^ civitas_lines)


(********************************* non-atelier windows **********************************)

let data staSnl wid = match wid with

(*****************************************************************************************************)
| W.Filters -> c "Tabula filters", List(Lines juce, [
  LB( K.KEY_f       , "F : open this window"    , [`wOpen (W.Filters, W.Default)           ] );
  LB( K.KEY_a       , "A : Artes"            , [`select_filter `artes                   ] );
  LB( K.KEY_d       , "D : Dominium"         , [`select_filter `dominium                ] );
  LB( K.KEY_i       , "I : Imperii"          , [`select_filter `imperii                 ] );
  LB( K.KEY_n       , "N : Nationes"         , [`select_filter `nationes                ] );
  LB( K.KEY_o       , "O : populatio"        , [`select_filter `populatio               ] );
  LB( K.KEY_p       , "P : Politeia"         , [`select_filter `politeia                ] );
  LB( K.KEY_r       , "R : Relief"           , [`select_filter `montes                  ] );
  LB( K.KEY_t       , "T : Tegmen"           , [`select_filter `tegmen                  ] );
  LB( K.KEY_v       , "V : Vis"              , [`select_filter `vis                     ] );
  ])
(*****************************************************************************************************)
| W.Game -> c "Game main menu", List(Lines juce, [
  LB( K.KEY_b       , "Begin a new game"              , [`wOpen (W.NewGame , W.Default) ] );
  LB( K.KEY_l       , "Load a previous game"          , [                               ] );
  LB( K.KEY_s       , "Save current state of the game", [                               ] );
  LB( K.KEY_r       , "Restore last saved state"      , [                               ] );
  LB( K.KEY_o       , "game Options"                  , [                               ] );
  LB( K.KEY_a       , "bAby mode (Ctrl+Alt+Home to cancel)", [ `switch_baby_mode true; `new_game { Game.resolution=E.Low }; `wClose W.Game  ] );
  LB( K.KEY_q       , "Quit humanitas"                , [`wOpen (W.Quit    , W.Default) ] );
  ])
(*****************************************************************************************************)
| W.Help -> c "Getting started", List(Lines juce, [
  LB( K.KEY_h       ,  "F1 : open this window"               , [`wOpen (W.Help    , W.Default)] );
  LB( K.KEY_d       ,  "D : restore Default display"         , [`defaultDisplay               ] );
  LB( K.KEY_g       ,  "G : Game main menu"                  , [`wOpen (W.Game    , W.Default)] );
  LB( K.KEY_k       ,  "K : Keyboard shorcuts list"          , [`wOpen (W.Keys    , W.Default)] );
  LB( K.KEY_m       ,  "M : Mouse hints"                     , [`wOpen (W.Mouse   , W.Default)] );
  LB( K.KEY_q       ,  "Q : Quit humanitas"                  , [`wOpen (W.Quit    , W.Default)] );
  ])
(*****************************************************************************************************)
| W.Keys   ->   
  let box  w a strn = Box (w, 1., a, S strn) in
  let line strn1 strn2 = List (Columns, [box 3. `right strn1 ; S "  :  " ; box 9. `left strn2] ) in
  c "Keyboard shortcuts", List(Lines juce, [
    line "K"   "open this window" ;
    line "Esc" "cancel focus" ;
    line "Tab" "focus/un-hide window" ;
    line "H"   "Hide all windows" ;
    line "Q"   "Quit" ;
    line "Ctrl+Alt+Home"  "Leave baby mode" ;
    line "Ctrl+Alt+End"   "Quit without confirm" ;
    S "---"  ;
    S "Map shortcuts"  ;
    line "+ or Z"   "zoom in" ;
    line "- or W"   "zoom out" ;
    line "E"   "toggle Earth mode" ;
    line "Home" "center map to Homeland" ;
    line "A"   "show Altitude" ;
    line "B"   "show Borders" ;
    line "N"   "show Nations" ;
    line "F"   "swith Filter" ;
(*    (SL None, [S " "    ; S "Window stack shortcuts"] );*)
    S "---"  ;
    S "Managing windows"  ;
    line "M"   "Move sheet" ;
    line "PgUp"  "Previous sheet" ;
    line "PgDown"  "Next sheet" ;
    line "U"   "Un-select stack / Undo" ;
    line "H"   "Hide stack" ;
    line "X"   "Remove sheet" ;
(*    line "Shift + X" "Close stack" ;*)
(*    (SL None, [S ""    ; S "Gates"] );*)
(*    line "C"   "Consilium" ;*)
  ])
(*****************************************************************************************************)
| W.NewGame -> c "New Game menu", List(Lines juce,
  let strn res = Strn.pth(soi(E.Cylinder.wir res)^" * "^soi(E.Cylinder.hir res)) in
  let task res = [`new_game { Game.resolution=res } ] in
  [
  S( "Pick a map resolution : " );
  LB( K.KEY_h  , "High "^(strn E.High), task E.High );
  LB( K.KEY_l  , "Low "^(strn E.Low), task E.Low );
  LB( K.KEY_r  , "loweR "^(strn E.Lower), task E.Lower );
  LB( K.KEY_c  , "Cancel"                  , [`wUndo  ] );
  ])
(*****************************************************************************************************)
| W.Quit -> c "Quit humanitas", List(Lines juce, [
  LB( K.KEY_q       , "Quit"            , [`quit                 ] );
  LB( K.KEY_c       , "Cancel"          , [`wUndo  ] );
  ])
(*****************************************************************************************************)
| W.TaskHistory -> c "Task History", List(Lines juce,
  let listSnl = rsm Status.task_history staSnl in
  let wsSnl   = rsm Status.windows staSnl in
  let awSnl   = rsm Windows.activeWindow wsSnl in
  let f i = Z(rsm (fun l -> (try (Si.task(List.nth l i) ) with Failure _ ->  "")) listSnl) in
  Z( rsm (fun wo -> "activeWindow : "^(match wo with None -> "None" | Some w -> Si.window w )) awSnl) ::
  LB( K.KEY_c       , "Click me"        , [                      ]) ::
  Tlist.init 8 f
  )
(*****************************************************************************************************)
| W.Time -> c "Hourglass of fate", List(Lines juce, [
  LB( K.KEY_F9      , "F9  : consult the archives"    , [`wOpen (W.Vetera  , W.Default)] );
  LB( K.KEY_F12     , "F12 : go to next event"        , [`next_event ] );
  LB( K.KEY_F10     , "F10 : open this menu"          , [`wOpen (W.Time    , W.Default)] );
  LB( K.KEY_RETURN  , "<return> : end of turn"        , [`end_of_turn 1                ] );
  LB( K.KEY_t       , "pass Ten years"                , [`end_of_turn 10 ] );
  LB( K.KEY_l       , " 50 (L) years"                 , [`end_of_turn 50 ] );
  LB( K.KEY_c       , "100 (C) years"                 , [`end_of_turn 100 ] );
  LB( K.KEY_d       , "500 (D) years"                 , [`end_of_turn 500 ] );
  LB( K.KEY_F12     , "F12 : go to next event"        , [`next_event ] );
  ])
(*****************************************************************************************************)
| _ -> c "Unknown window", List(Lines juce, [
  LB( K.KEY_c       , "Click me"        , [                      ]);
]) 

(*****************************************************************************************************)

let contents_s status_s wid =
  let module RSAO = RS.Make(struct type 'a t = SA.t option let equal = ( == ) end) in
  let atelier_s = RSAO.map Status.atelier status_s in match wid with
  | W.Artes
  | W.Chora
  | W.Dx 
  | W.Fines
  | W.Partitio
  | W.Polis 
  | W.Tactics
  | W.Pyramid -> ( let f = function
        | Some a ->( 
               let nid = Game.Player.pov (SA.player a) in
               if nid = Nid.none then None 
               else Some (wid, natio a nid wid)
               )
        | _ -> None in RS.map f atelier_s )
  | W.Regio -> ( let f = function
        | Some a -> ( match Scene.sr (SA.scene a) with
                   | Some rid -> Some (W.Regio, regio a rid)
                   | _ -> None )
        | _ -> None in RS.map f atelier_s)
  | W.Tabula
  | W.Newspaper
  | W.Vetera
  | W.Orbis -> ( let f = function
      | Some a -> Some (wid, atelier a wid)
      | _ -> None in RS.map f atelier_s)
  | id -> RS.const (Some (id, data status_s id))


(* EOF *)
