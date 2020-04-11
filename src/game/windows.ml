(*

 ****************************** Windows.ml ******************************


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

(* fonctions relatives à un game.status *)

open Humanitas_tools
open Std
module W   = WindowID
module B   = Button

(*module WList = Ilist (struct type key=W.t end) (*Window ID indexed List*)*)

(*type scrollPos =
| Top of int
| Bottom of int*)

type stackStatus = W.status

(*type buttonAlert =*)
(*| Awake*)
(*| Numb*)
(*| Asleep*)

type t =
  {
  leftStack : W.t list; (*fenêtres vivantes situées dans la pile de gauche, la première est seule visible, les
  autres sont cachées *)
  rightStack : W.t list;
  towers : W.tower list;
  queenHistory : W.t list; (*fenêtre occupant la position centrale + historique des précédentes *)
  focus : W.position option; (*le focus est à gauche, à droite, sur la queen, ou nulle part *)
  leftStackStatus : stackStatus; 
  rightStackStatus : stackStatus;
  }

let queen  ws          = match ws.queenHistory with t::_q -> Some t | [] -> None
let towers ws          = ws.towers
let leftStack ws       = ws.leftStack
let rightStack ws      = ws.rightStack
let leftStackStatus ws = ws.leftStackStatus
let rightStackStatus ws= ws.rightStackStatus

let windowPos ws wName = 
  if List.mem wName ws.queenHistory then W.Central
  else if List.mem wName ws.leftStack then W.Left
  else W.Right

let activeWindow ws =
  match ws.focus with
  | Some W.Central -> queen ws
  | Some W.Left    -> (match ws.leftStack  with h::_q -> Some h | _ -> None)
  | Some W.Right   -> (match ws.rightStack with h::_q -> Some h | _ -> None)
  | _              ->                                                 None

(*let is_window_focused ws wName = if (activeWindow ws = Some wName) then W.Active else W.Inactive*)

let windowState ws wid = match W.duty wid with
  | W.Tower -> W.Alive (* if List.mem wid (towers ws)           then    W.Alive else   W.Invisible *)
  | W.Queen -> (match ws.queenHistory with t::_q when t=wid -> W.Alive | _ -> W.Nil)
  | W.Sheet -> (match ws.leftStack    with 
        | t::_q                                  when t=wid -> W.Alive
        | _  -> (match ws.rightStack   with t::_q when t=wid -> W.Alive | _ -> W.Nil))


(*let  ws wName = *)

(* let buttonAlert ws (wid,bid) = *)
(*   let aw = activeWindow ws in *)
(*   match bid with *)
(*   | B.Tb when aw = W.None -> Awake *)
(*   | B.Tb                  -> Numb *)
(*   | _    when aw = wid    -> Awake *)
(*   | _                     -> Asleep *)
(* (\* État d’éveil des buttons = réactivité aux raccourcis clavier *\) *)

(*let sceneCenterXDelta ws = 
  let isLeftStack  = (ws.leftStack<>[] && ws.leftStackStatus=Alive)
  and isRightStack = (ws.rightStack<>[] && ws.rightStackStatus=Alive) in
  let hw = (Screen.ewip*3/2) in
  match isLeftStack, isRightStack with (*actual delta*)
  | true, false -> ( hw)
  | false, true -> (-hw)
  | _           ->    0*)
(* Le centre de la scène dépend de l’existence de stack de part et d’autres *)

(***************************** WINDOWS.CREATE ***********************************)

let create () =
  {
  leftStack = []; (*[W.TaskHistory];*)
  rightStack = []; (*[W.Regio];*)
  towers=W.towers;
  queenHistory=[];
  focus=None;
(*  pressedButton=(W.None, B.None);*)
  leftStackStatus  = W.Alive;
  rightStackStatus = W.Alive;
(*  scrollPosList = WList.null;*)
  }

(***************************** WINDOWS.UPDATE **********************************)

let putTheSheetBelow = function
| e::q -> q^^[e]
| []   -> []
(* met la feuille du dessus sous la pile *)

let rec takeTheSheetBelow head list = match list with
| last::[] -> last::(List.rev head)
| e::q     -> takeTheSheetBelow (e::head) q 
| []       -> []
(* prend la dernière feuille de la pile, et mets la sur la pile *)

let feuillette ws sens =
  let f = match sens with
  | `wPrevious -> takeTheSheetBelow []
  | _          -> putTheSheetBelow in
  match ws.focus with
  | Some W.Left -> { ws with leftStack = f ws.leftStack }
  | Some W.Right-> { ws with rightStack= f ws.rightStack}
  | _           ->   ws
(* parcourir une stack *)

let sheetSide name side = match side with
| W.Central ->  W.Central
| W.Left    ->  W.Left
| W.Right   ->  W.Right
| W.Default ->  W.defSide name
| W.Valid   ->  W.defSide name
| W.Alter   -> (W.defSide name=W.Left => W.Right) W.Left
(* ou l’on doit créer une window *)

let focus ws pos =
  let focusOrder = [W.Left; W.Central; W.Right] in
  let isFocusValid = function
  | W.Central -> (queen ws    <> None)
  | W.Left    -> (ws.leftStack  <> [])
  | W.Right   -> (ws.rightStack <> []) 
  | _         -> false in
  let rec nextValidFocus i focus =
    let next = Tlist.following_or_first focus focusOrder in
    if isFocusValid next then Some next
    else if i=3          then None
    else nextValidFocus (i+1) next in
  match ws.focus, pos with
  | _     , Some W.Central -> Some W.Central
  | _     , Some W.Left    -> Some W.Left
  | _     , Some W.Right   -> Some W.Right
  | None  , Some W.Alter
  | None  , Some W.Valid   -> nextValidFocus 0 W.Right
  | Some f, Some W.Alter   -> nextValidFocus 0 f
  | Some f, Some W.Valid   -> if isFocusValid f then Some f else nextValidFocus 0 f
  | _                      -> None
(* déterminer un nouveau focus en fonction de la pos demandée *)

let wOpen ws (name,side) = match (W.duty name),(sheetSide name side) with
  | W.Sheet, W.Left -> { ws with rightStack =          Tlist.remove ws.rightStack name;
                                 leftStack  = name :: (Tlist.remove ws.leftStack  name);
                                 leftStackStatus  = W.Alive;
                                 focus      = Some W.Left }
                                 (* la nouvelle doit être sur la pile *)
  | W.Sheet, W.Right-> { ws with leftStack  =          Tlist.remove ws.leftStack  name;
                                 rightStack = name :: (Tlist.remove ws.rightStack name);
                                 rightStackStatus = W.Alive;
                                 focus      = Some W.Right }
  | W.Tower, _      -> ws
                       (* if List.mem name ws.towers then ws else *)
                       (* { ws with towers = name :: ws.towers } *)
  | W.Queen, _      -> { ws with queenHistory = (match ws.queenHistory with
                                                 | t::q when name=t -> name::q
                                                 | list             -> name::list);
                                 focus  = Some W.Central }
  | _ -> ws
(* ajouter une fenêtre *)

let wClose ws name = match W.duty name with
  | W.Sheet -> let ws = { ws with rightStack = Tlist.remove ws.rightStack name; 
                                  leftStack  = Tlist.remove ws.leftStack  name } in
                        { ws with focus      = focus ws (*le nouveau avec la fenetre en moins*) (Some W.Valid) } 
  | W.Tower -> ws (* { ws with towers     = Tlist.remove ws.towers     name } *)
  | W.Queen -> { ws with queenHistory = [] }
(* fermer une fenêtre *)

let wMove ws =
  let new_side = match ws.focus with
  | Some W.Left  -> Some W.Right
  | Some W.Right -> Some W.Left
  | _            -> None in
  match new_side with
  | None -> ws
  | Some new_side -> match activeWindow ws with 
                     | Some sheet -> let ws = wClose ws sheet in wOpen  ws (sheet, new_side)
                     | _          -> ws
(* déplacer une sheet sur l'autre stack *)


let toggle_stacks ws =
  if leftStack  ws<>[] && leftStackStatus  ws=W.Alive
  || rightStack ws<>[] && rightStackStatus ws=W.Alive
  then { ws with leftStackStatus=W.Glass;
                 rightStackStatus=W.Glass }
  else { ws with leftStackStatus=W.Alive;
                 rightStackStatus=W.Alive }
(* cache ou affiche les stacks *)

let update ws command =
  let ws = match command with
| `defaultDisplay      -> create()
| `toggle_stacks       -> toggle_stacks ws
| `wOpen (name,side)   -> wOpen ws (name,side)
| `wClose (name)       -> wClose ws name
| `wPrevious   
| `wNext               -> feuillette ws command
| `wUndo               -> { ws with queenHistory = match ws.queenHistory with _t::q -> q | _ -> [] }
| `wMove               -> wMove ws
| `sFocus(pos)         -> let newFocus = focus ws pos in ( match newFocus with
                          | Some W.Left  -> { ws with leftStackStatus  = W.Alive; focus=newFocus }
                          | Some W.Right -> { ws with rightStackStatus = W.Alive; focus=newFocus }
                          | _            -> { ws with focus=newFocus } )
| `sHide(pos)          -> ( match pos with 
                          | W.Left  -> { ws with leftStackStatus  = W.Glass }
                          | W.Right -> { ws with rightStackStatus = W.Glass }
                          | _       ->   ws)
| `sClose(pos)         -> ( match pos with 
                          | W.Left  -> { ws with leftStack  = [] }
                          | W.Right -> { ws with rightStack = [] }
                          | _       ->   ws)
| _ -> ws in
  match ws.focus with
| Some W.Left    when (ws.leftStack =[] || ws.leftStackStatus =W.Glass) -> { ws with focus=None }
| Some W.Right   when (ws.rightStack=[] || ws.rightStackStatus=W.Glass) -> { ws with focus=None }
| Some W.Central when  ws.queenHistory=[]                              -> { ws with focus=None }
| _ -> ws
(* suppression des focus non pertinents et non visibles *)
  

(* EOF *)
