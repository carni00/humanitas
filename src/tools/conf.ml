let table = ref []

let load_rc_file path =
  let ic = open_in path in
  (try while true do
     let l = input_line ic in 
     Scanf.sscanf l "%s = %s" (fun k v -> table := (k,v) :: !table)
    done with End_of_file -> ()) ;
  close_in ic

let home_dir = Sys.getenv "HOME"
let default_rc_file = home_dir ^ "/.humrc" 

let _ = 
  if Sys.file_exists default_rc_file
  then load_rc_file default_rc_file
(* chargement dans table des données du fichier rc *)

(**************)

let get f var default = 
  try f (List.assoc var !table)
  with Not_found -> default
(* lecture sécurisée de la table *)

let string var default = get (fun x -> x) var default
(* lecture de la table, sortie en string *)
let int var default = get int_of_string var default
(* lecture de la table, sortie en int *)
let bool var b = get (fun s -> List.mem s [ "true" ; "yes" ; "y" ; "1" ; "enabled" ]) var b
(* lecture de la table, sortie en bool *)

(**************)

let _ = print_endline ("~/.humrc "^(string "TEST" "not_found"))
(* confirmation de l’obtention du humrc *)
let _ = print_endline ("full screen "^(string "FULLSCREEN" "default"))
let _ = print_endline ("screen native resolution wip (SCRWIP) "^(string "SCRWIP" "default"))
let _ = print_endline ("screen native resolution hip (SCRHIP) "^(string "SCRHIP" "default"))
let _ = print_endline ("full screen resolution wip (FRWIP) "^(string "FRWIP" "default"))
let _ = print_endline ("full screen resolution hip (FRHIP) "^(string "FRHIP" "default"))
let _ = print_endline ("window mode inside wip (WMIHIP) "^(string "IHIP" "default"))
let _ = print_endline ("window mode inside hip (WMIHIP) "^(string "IHIP" "default"))
let _ = print_endline ("bitmaps location (BMP) "^(string "BMP" "default"))
(* affichage des données de config retenue pour l’exécution *)
