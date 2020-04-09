(** Identifiant d'une {i natio} 

    Ce module rassemble également les informations qui font l'identité d'une 
    nation.
*)

open Humanitas_tools
open Std

type t = [`natio_id] id

type 'a assoc = (t * 'a) list
