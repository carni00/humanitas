(*

 ****************************** src/game/si.ml ******************************


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
open Humanitas_physis
open Humanitas_orbis
open Humanitas_tools
open Std
(** concept to_string *)

module W = WindowID

type natio = { adj : string; regio : string; prin : string; civitates : string list; }

let natioList = [
{ adj="Native"   ;regio="Earth"   ;prin="Nobody"      ; civitates=["A";"B";"C";"D"]; };
{ adj="Egyptian" ;regio="Aegyptus";prin="Kheops"      ;civitates=["Memphis"   ;"Thebes"     ;"Akhetaton";"Pi-Ramses" ]; };
{ adj="Akkadian" ;regio="Akkad"   ;prin="Sargon"      ;civitates=["Nippour"   ;"Kish"       ;"Mari"     ;"Babylone"   ]; };
{ adj="Indian"   ;regio="Sindhu"  ;prin="Açoka"       ;civitates=["Harappa"   ;"Pataliputra";"Vijayanagara";"Dehli"   ]; };
{ adj="Assyrian" ;regio="Assyria" ;prin="Assurbanipal";civitates=["Assur"     ;"Kalkhu" ;"Dur-Sharrukin";"Ninive"     ]; };
{ adj="Greek"    ;regio="Ellas"   ;prin="Alexandre"   ;civitates=["Knossos"   ;"Mukenai"    ;"Sparta"   ;"Athinai"    ]; };
{ adj="Persian"  ;regio="Iran"    ;prin="Cyrus"       ;civitates=["Persepolis";"Pasargades" ;"Ectabane" ;"Ispahan"    ]; };
{ adj="Punic"    ;regio="Africa"  ;prin="Hannibal"    ;civitates=["Karthago"  ;"Hadrumete"  ;"Utique"   ;"Gadès"      ]; };
{ adj="Celt"     ;regio="Gallia"  ;prin="Brennos"     ;civitates=["Hallstatt" ;"La Tene"    ;"Cenabum"  ;"Tara"       ]; };
{ adj="Roman"    ;regio="Italia"  ;prin="Caesar"      ;civitates=["Roma"      ;"Veies"      ;"Antium"   ;"Mediolanum" ]; };
{ adj="Han"      ;regio="Zhonghua";prin="Liu Bang"    ;civitates=["Changan"   ;"Luoyang"    ;"Kaifeng"  ;"Beijing"    ]; };
{ adj="Ethiopian";regio="Ethiopia";prin="Ezana"       ;civitates=["Aksoum"    ;"Yeha"       ;"Méroé"    ;"Gondar"     ]; };
{ adj="Maya"     ;regio="Yucatan" ;prin="Pacal"       ;civitates=["Calakmul"  ;"Tikal"      ;"Palenque";"Chichen Itza"]; };
{ adj="Viking"   ;regio="Norge"   ;prin="Knut"        ;civitates=["Haithabu"  ;"Birka"      ;"Ribe"     ;"Trondheim"  ]; };
{ adj="Khmer"    ;regio="Funan"   ;prin="Jayavarman"  ;civitates=["Vyadhapura";"Ishanapura" ;"Hariharalaya";"Angkor"  ]; };
{ adj="Songhai"  ;regio="Mali"    ;prin="Soni Ali"    ;civitates=["Gao"       ;"Tombouctou" ;"Niani"    ;"Djenné"     ]; };
{ adj="Uzbek"    ;regio="Sogdiane";prin="Timur"       ;civitates=["Samarkand" ;"Bokhara"    ;"Nishapur" ;"Tabriz"     ]; };
{ adj="Aztec"    ;regio="Aztlan"  ;prin="Moctezuma"   ;civitates=["Tenochtitlan";"Chapultepec";"Texcoco";"Tlacopan"   ]; };
{ adj="Inca"; regio="Tawantinsuyu";prin="Pachacutec"  ;civitates=["Cuzco"     ;"Machu Pichu";"Tihuanacu";"Vilcabamba" ]; };
{ adj="Zoulou"   ;regio="Mutapa"  ;prin="Chaka"       ;civitates=["Zimbabwe"  ;"Ulundi"    ;"Isandlwana";"Tugela"     ]; };

]
(* classées par convention par ordre chronologique *)

type natioForm =
| Name
| Apoc 
| Adj
| Regio
| Civitas of int

let natio natioForm nid =
  let tab = Array.of_list natioList in
  if nid = Nid.none then "none" else 
  let base = tab.(Nid.ti nid) in
  match natioForm with
  | Name  -> (match base.adj with 
             | "Chinese" -> "Chinese"
             | _         -> base.adj^"s")
  | Apoc -> String.sub base.adj 0 3
  | Adj   -> base.adj
  | Regio -> base.regio
  | Civitas n -> if n>=0 && n<4 then List.nth base.civitates n else base.regio^" no"^(soi n)

let natioName             = natio Name
let natioApoc             = natio Apoc
let natioAdj              = natio Adj
let natioRegio            = natio Regio
let natioCivitas ~nid ~i  = natio (Civitas i) nid
let civitas cvt           = match Civitas.name_key cvt with nid,i -> natioCivitas ~nid ~i

let ars = function
| Ars.AGR -> "agriculture"
| Ars.MET -> "metallurgy"
| Ars.WRI -> "writing"
| Ars.NAV -> "navigation"
| Ars.GUN -> "gunpowder"
| Ars.STE -> "steam engine"
| Ars.CMB -> "combustion"
| Ars.ELE -> "electricity"
| Ars.N_ARS -> "nulla ars"

let actio = function
| Eventum.Actio.Civitas    cvt      -> "found "    ^ civitas cvt
| Eventum.Actio.Inventio   ars_id   -> "discover " ^ ars ars_id
| Eventum.Actio.Propagatio ars_id   -> "acquire "  ^ ars ars_id
(*| Eventum.Actio.Mutatio    politeia ->*)
| Eventum.Actio.Bellum     nid      -> "declare war on "  ^ natioName nid
| Eventum.Actio.Offensive  nid      -> "attack "          ^ natioName nid
| Eventum.Actio.Pax        nid      -> "make peace with " ^ natioName nid

let date = function
| Date.G g -> let g = (g:>int) in if g<0 then soi(-g)^" BCE" else soi g ^" CE"
| Date.N _ -> "national date"
| Date.Unknown -> "unknown date"

let eventum evt =
  let actio  = actio     (Eventum.actio  evt) in
  let acteur = natioName (Eventum.acteur evt) in
  let date   = date      (Eventum.date   evt) in
  date ^ " : " ^ acteur ^ " " ^ actio


let regio e rid = 
  let lat,lon = Espace.Regio.coords e rid in
  (*"Regio "^*)(Strn.coords lat lon)


let role = function
  | Game.Player.Admin -> "Admin"
  | Game.Player.Basileus _ -> "Basileus"
  | Game.Player.Civis    _ -> "Civis"
  | Game.Player.Spectator -> "Spectator"



 let window = function
(* | W.Esc         -> "Esc"      *)
(* | W.Filter      -> "Filter"         *)
(* | W.Save        -> "Save"       *)
(* | W.TaskHistory -> "TaskHistory"              *)
(* | W.Coords      -> "Coords"         *)
(* | W.Fps         -> "Fps"      *)
(*| W.Gate w      -> "Gate"^window w         *)
| W.Towers      -> "Towers"
| W.Chora       -> "Chora"
| W.Computing   -> "Computing"
| W.Help        -> "Getting Started"      
| W.Game        -> "Humanitas"      
| W.Display     -> "Display"         
| W.Dx          -> "Dx"         
| W.Natio       -> "Natio"        
| W.Imperium    -> "Imperium"          
| W.Time        -> "Time"      
| W.Fines       -> "Fines"      
| W.Consilium   -> "Consilium"           
| W.Polis       -> "Polis"       
| W.Humanitas   -> "Humanitas"           
| W.Keys        -> "Keyboard shortcuts"      
| W.Mouse       -> "Mouse hints"       
| W.Regio       -> "Regio"       
| W.Quit        -> "Quit"       
| W.NewGame     -> "NewGame"       
| W.Orbis       -> "Orbis"       
| W.ContactusMx -> "ContactusMx"       
| W.Pyramid     -> "Age structure"       
| W.Artes       -> "Discoveries"       
| W.Partitio    -> "Annual products and incomes"
| W.TaskHistory -> "Task history"
| W.Tactics     -> "Tactics"
| W.Vetera     -> "Vetera"

let position = function
| W.Default -> "Default"  
| W.Alter   -> "Alter"
| W.Left    -> "Left" 
| W.Right   -> "Right"
| W.Central -> "Central"  
| W.Valid   -> "Valid"  

let task = function
| `do_nothing       -> "nothing"
| `new_game _       -> "new_game _    "
| `end_of_turn _    -> "end_of_turn _    "
| `next_event       -> "next_event "
(*| `map_center _     -> "map_center _  "*)
(*| `map_move _       -> "map_move _  "*)
| `move_sr  _       -> "move_sr  _  "
| `select_regio _   -> "select_regio _"
| `wClose w         -> "wClose"^" "^(window w)
| `wOpen (w,p)      -> "wOpen"^" "^(window w)^" ("^(position p)^")"
| `wNext            -> "wNext         "
| `wPrevious        -> "wPrevious     "
| `wUndo            -> "wUndo         "
| `sFocus p         -> "sFocus"^" "^(match p with None->"None" | Some p -> position p)
| `sHide  _         -> "sHide  _      "
| `sClose _         -> "sClose _      "
(*| `bPress (w,b)     -> "bPress"^"("^(window w)^", "^(button b)^")"*)
(*| `bRelease         -> "bRelease      "*)
| `defaultDisplay   -> "defaultDisplay"                  
(*| `zoom_in          -> "zoom_in"           *)
(*| `zoom_out         -> "zoom_out"            *)
(*| `angle_up         -> "angle_up"            *)
(*| `angle_down       -> "angle_down"            *)
(*| `toggle_earthMode -> "toggle_earthMode"                    *)
(*| `select_filter _  -> "select_filter"                 *)
(*| `switch_filter    -> "switch_filter"                 *)
(*| `toggle_grid      -> "toggle_grid"               *)
(*| `toggle_polyhedron-> "toggle_polyhedron"                     *)
(*| `toggle_stacks    -> "toggle_stacks"                 *)
| _                 -> "?t"

let altitude a = match a with
| 0 -> "sea level"
| 1 -> "plain"
| a -> (soi a)^" m"

let climat = function
| R.Arid       ->"arid"
| R.Semiarid   ->"semiarid"
| R.Tropical   ->"tropical"
| R.Equatorial ->"equatorial"
| R.Temperate  ->"temperate"
| R.Subarctic  ->"subarctic"
| R.Arctic     ->"arctic"
| R.Alpine     ->"alpine"
| R.Subalpine  ->"subalpine"

let river d = if d=0 then " " else let s = Strn.longInt 3 d in ((s="0" => "river") "flumen")
let debit d = if d=0 then " " else let s = Strn.longInt 3 d in ((s="0" => "<1 000") s)^" m3/s"
(* débit d’un fleuve *)
    
let plebs p         = if p=0. then "none" else Strn.rdblfloat 3  p^" people"
let population p rs = if p=0. then "none" else Strn.float  (-1) (p /. rs)^" per km2"

let hydros = function 
| R.Inlandsis -> "inlandsis"
| R.SeaIce    -> "sea ice"
| R.Ocean     -> "ocean"
| R.Sea       -> "sea"
| R.Lake      -> "lake"
| R.River _   -> "river"
| R.Dry       -> "dry"


let climax = function
  | R.Desert          -> "desert"
  | R.Steppe          -> "steppe"
  | R.Grassland R.Savanna -> "savanna"
  | R.Grassland R.OtherG  -> "grassland"
  | R.Woodland R.Taiga      -> "taiga"
  | R.Woodland R.Coniferous -> "woodland (coniferous)"
  | R.Woodland R.Deciduous  -> "woodland (deciduous)"
  | R.Forest   R.Deciduous  -> "forest   (deciduous)"
  | R.Woodland R.TropicalF  -> "dry tropical forest"
  | R.Forest R.RainF      -> "rainforest"
  | R.Tundra R.ArcticT    -> "arctic tundra"
  | R.Tundra R.AlpineT    -> "alpine tundra"
  | _                 -> "unknown"


let tegmen = function
  | Rv.Hydros h      -> hydros h
  | Rv.Desertum natura -> climax natura
  | Rv.Fields_and_woods->"Fields_and_woods"
  | Rv.Fields_and_pasture->"Fields_and_pasture"
  | Rv.Pasture->"Pasture"
  | Rv.Fields->"Fields"
  | Rv.Irrigation->"Irrigation"
  | Rv.Tmine->"Mine"
  | Rv.Turbs->"Urbs"

let dominium = function
  | Rv.Incola.Mir          -> "Mir" (* communauté paysanne *)
  | Rv.Incola.Latifundium  -> "Latifundium" (* grande propriété privée *)
  | Rv.Incola.Minifundium  -> "Minifundium" (* petites propriétés privées *)




let filter = function
| `artes    -> "artes"       
| `dominium -> "dominium"       
| `nationes -> "nationes"          
| `imperii  -> "imperii"         
| `montes   -> "montes"        
| `populatio-> "populatio"        
| `politeia -> "politeia"        
| `vis      -> "vis"        
| `tegmen   -> "tegmen"        

let natioKey = function
  | Natio.Facultas        -> "Facultas"           
  | Natio.Plebs           -> "Plebs"           
  | Natio.Hospitalitas    -> "Hospitalitas"           
  | Natio.Instrumentum    -> "Instrumentum"           
  | Natio.Efficientia     -> "Efficientia"           
  | Natio.Famine          -> "Famine"           
  | Natio.Copia           -> "Copia"           
  | Natio.Tfg             -> "Tfg"           
  | Natio.Isf             -> "Isf"           
  | Natio.DxVar           -> "DxVar"           
  | Natio.Alimonium_ratio -> "Alimonium_ratio"              
  | Natio.Facultas_ratio  -> "Facultas_ratio"              
  | Natio.Sophia          -> "Sophia"           
  | Natio.Fides           -> "Fides"           
  | Natio.Libertas        -> "Libertas"           
  | Natio.AgriCopia       -> "AgriCopia"           
  | Natio.Densitas        -> "Densitas"           
(*  | _                     -> "natioKey"           *)
              

let arsID = function
| Ars.AGR -> "AGR"
| Ars.MET -> "MET"
| Ars.WRI -> "WRI"
| Ars.NAV -> "NAV"
| Ars.GUN -> "GUN"
| Ars.STE -> "STE"
| Ars.CMB -> "CMB"
| Ars.ELE -> "ELE"
| Ars.N_ARS -> "NUL"

let attributio = function
| Partitio.LAB -> "labor"
| Partitio.SAP -> "sapientia"
| Partitio.REL -> "religio"
| Partitio.MIL -> "militaria"
| Partitio.OPP -> "oppressio"
| Partitio.LUX -> "luxus"
| Partitio.OTI -> "otium"

let offensive = function
| Junctiones.Release -> "Release"
| Junctiones.Conquest -> "Conquest"

let tactic = function
| Junctiones.Offensive off -> "Off. ("^(offensive off)^")"
| Junctiones.Defensive -> "Def. (Defensive)"
| Junctiones.Retreat   -> "Ret. (Retreat)  "

