(** 
    Access to configuration files 

    This modules tries to read a configuration file at startup in standard locations,
    and provides reading functions on their contents. The expected syntax of configuration
    files are a sequel of lines s.t.
      lines ::= [KEY] = [VALUE]
*)

val int : string -> int -> int
(** [int strn def] returns the integer associated with key [strn] ; returns default if there is none *)

val string : string -> string -> string
(** reads a string *)

val bool : string -> bool -> bool
(** reads a boolean *)
