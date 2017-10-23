open Sdlvideo

external line : surface -> 
  x1:int -> y1:int -> 
  x2:int -> y2:int -> int32 -> unit
    = "ml_SDL_line_bytecode" "ml_SDL_line_native"
external triangle : surface -> 
  x1:int -> y1:int -> 
  x2:int -> y2:int -> 
  x3:int -> y3:int -> int32 -> unit
    = "ml_SDL_triangle_bytecode" "ml_SDL_triangle_native"
external filled_triangle : surface -> 
  x1:int -> y1:int -> 
  x2:int -> y2:int -> 
  x3:int -> y3:int -> int32 -> unit
    = "ml_SDL_filled_triangle_bytecode" "ml_SDL_filled_triangle_native"
external text_filled_triangle : surface -> 
  x1:int -> y1:int -> 
  x2:int -> y2:int -> 
  x3:int -> y3:int -> 
  int32 -> int ->int ->unit
    = "ml_text_filled_triangle_bytecode" "ml_text_filled_triangle_native"
