open Batteries
open Std

let mouse_event_of_irr mi = Irr_base.({
  UI.m_left = mi.mi_left_pressed ;
  m_middle = mi.mi_middle_pressed ;
  m_right = mi.mi_right_pressed ;
  m_x = mi.mi_x ;
  m_y = mi.mi_y
})

let send_event = ref ignore 

let on_event = function
  | `mouse_input mi -> !send_event (`mouse (mouse_event_of_irr mi)) ; false
  | _ -> false

let device = Irr.create_device ~fullscreen:true ~dtype:`opengl ~on_event ~size:Screen.(wip,hip) ()

let driver = device#driver

let width = React.S.const (float Screen.wip)
let height = React.S.const (float Screen.hip)

let irrcolor a col = 
  let a,r,g,b = Color.to_irr a col in
  Irr_core.color_ARGB ~a ~r ~g ~b 

let irrrect box = 
  iof box.UI.left - 1, iof box.UI.top - 1, iof box.UI.right, iof box.UI.bottom

let draw_rect ?(alpha = 1.) col box = V2.(
  driver#draw_2d_rect (irrcolor alpha col) (irrrect box)
)

type font = Irr_gui.font

let default_font = device#gui_env#font "gfx/fontlucida.png"

let text_irrrect box =
  iof box.UI.left - 1, iof box.UI.top - 1, iof box.UI.right, iof box.UI.bottom

let draw_text ?(pos : [`left | `right | `centered] = `centered) (font : font) msg box =
  font#draw ~hcenter:true ~vcenter:true msg (text_irrrect box) (irrcolor 1. Color.white)

let text_size font msg = 
  let x, y = font#get_dimension msg in
  foi x, foi y

module Button = struct
  type style = unit
  let default = ()
  let size style text = 
    let (w, h) = text_size default_font text in
    (w +. 6., h +. 2.)
  let draw style text pressed pos =
    draw_rect (if pressed then Color.yellow else Color.red) pos ;
    draw_text default_font text pos

end
