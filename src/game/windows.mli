(*

 ****************************** windows.mli ******************************


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

(** État courant de l’organisation générale de l’interface

    L’interface est faite de fenêtre, lesquelles peuvent contenir des
    boutons. Les fenêtres peuvent être ouvertes, fermées, focussées, ou
    inactivées par l’utilisateur.

    Il existe trois trois types de fenêtres :
    - les towers : fenêtres composées d’un bouton unique. Elles sont
      situées alignées tout en haut et tout en bas de l’écran. Leur
      position potentielle est invariable.
    - les sheets : au format A4, elles s’accumulent dans des piles,
      dans les tiers gauche, et droit, de l’écran.
    - les queen : de format variable, elles se succèdent au centre de
      l’écran.

*)




type t

type stackStatus = WindowID.status

(*   type buttonAlert = *)
(*   | Awake    (\** le bouton réagit à tous les raccourcis clavier *\) *)
(*   | Numb     (\** le bouton ne réagit qu’au touches F1..F12 *\) *)
(*   | Asleep   (\** le bouton ne réagit à aucun appel clavier *\) *)
(* (\** état de réactivité d’un bouton au raccourcis clavier *\) *)
  
  val create : unit -> t

  val update : t -> Task.t -> t
  
(** {5 Fonctions de lecture de l’état courant de l’interface }*)

  val queen            : t -> WindowID.t option
(** renvoie la fenêtre en position centrale, s’il y en a une *)

  val towers           : t -> WindowID.tower list
(** les boutons disposés tout en haut et tout en bas de l’écran *)

  val leftStack        : t -> WindowID.t list 
(** la pile de fenêtre de gauche *)

  val rightStack       : t -> WindowID.t list

  val leftStackStatus  : t -> stackStatus 
(** le statut : visible ou cachée, de la pile de gauche *)

  val rightStackStatus : t -> stackStatus


  (* val buttonAlert  : t -> (WindowID.t*Button.name) -> buttonAlert *)

  val activeWindow : t                             -> WindowID.t option
(** fenetre active = focussée = une seule *)

  val windowState : t  ->  WindowID.t -> WindowID.status
(*  val is_window_focused : t ->  WindowID.t              -> WindowID.status*)
(* | Active
| Inactive
| Hidden
| Killed*)

  val windowPos    : t ->  WindowID.t -> WindowID.position


