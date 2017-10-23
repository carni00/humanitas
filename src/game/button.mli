(*

 ****************************** button.ml ******************************


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

type name =
| None
| C 
| P (* previous window *)
| N (* next     window *)
| U (* unselect window *)
| H (* hide     window *)
| X (* x-it window *)
| Cancel
| Tb (* Les towers n’ayant au max qu’un button, il n’est pas utile de les distinguer *)
| Gate of WindowID.t

| Help
| Keys 
| Load
| Mouse
| Normal
| Options
| Quit 
| Restore
| Save
| Small
| Time 
| Tiny

| Default
| Zoom
| Unzoom
| Earth
| Filter
| Montes
| Nationes
| Regio
| Grid
| Polyhedron
| Stacks
| Capitolium



(*type status =
| Pressed
| Released*)

type t = {
  name   : name;
  title  : string;
  }


