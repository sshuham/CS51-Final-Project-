open Core.Std
(** A helper module for drawing graphics. *)

(** Draws a circle with lower left corner at position (x,y) with the specified
    width and height.  The circle is drawn with color bg and the text t is
    drawn with color fg on top of the circle. *)
let circle ((x,y):int*int) (width:int) (height:int)
           (bg:Graphics.color) (fg:Graphics.color) (t:string) : unit =
  Graphics.set_color bg ;
  Graphics.fill_circle (x*width + width/2) (y*height + height/2)
                       (min width height / 2) ;
  Graphics.moveto (x*width+2) (y*height) ;
  Graphics.set_color fg ;
  Graphics.draw_string t

let rect ((x,y):int*int) (width:int) (height:int)
	 (bg:Graphics.color) : unit =
  Graphics.set_color bg ;
  Graphics.fill_rect (x*width + width/2) (y*height + height/2)
                       (width / 2) (height / 2);
  Graphics.moveto (x*width+2) (y*height) ;
