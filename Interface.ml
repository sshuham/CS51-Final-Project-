open Core.Std
open Graphicevents

(********************)
(***** UI State *****)
(********************)

let mouse_state = ref false
let mouse_pos = ref (0,0)
let board_width = 70
let board_height = 60 

(*********************)
(***** UI Events *****)
(*********************)

(** Fires when a key is pressed and returns the character corresponding
    to the key. *)
let key_pressed : char Grapchicevents.event = Graphicevents.new_event ()

(** Fires when the mouse button is pressed, indicating the coordinates
    where the mouse was when the event occurred. *)
let button_down : (int*int) Graphicevents.event = Graphicevents.new_event ()

(** Fires when the mouse button is released, indicating the coordinates
    where the mouse was when the event occurred. *)
let button_up : (int * int) Graphicevents.event = Graphicevents.new_event ()

(** Fires when the mouse moves, indicating the coordinates where the
    mouse was when the event occured. *)
let mouse_motion : (int * int) Graphicevents.event = Graphicevents.new_event ()

(************************)
(***** Event System *****)
(************************)

(* poll the Graphics module for the various events -- some care had to
   be taken to "de-bounce" the mouse. *)
let read_event () =
  let new_pos = Graphics.mouse_pos () in
  if new_pos <> !mouse_pos then begin
    mouse_pos := new_pos ;
    Graphicevents.fire_event mouse_motion (Graphics.mouse_pos ())
  end ;
  if Graphics.key_pressed () then begin
    Graphicevents.fire_event key_pressed (Graphics.read_key ())
  end ;
  if not !mouse_state then begin
    let s = Graphics.wait_next_event [Graphics.Button_down ; Graphics.Poll] in
    if s.Graphics.button then begin
      mouse_state := true ;
      Graphicevents.fire_event button_down new_pos
    end
  end ;
  if !mouse_state then begin
    let s = Graphics.wait_next_event [Graphics.Button_up ; Graphics.Poll] in
    if not s.Graphics.button then begin
      mouse_state := false ;
      Graphicevents.fire_event button_up new_pos
    end
  end ;

(* Helper for restarting interrupted system calls (OY) *)
let rec restart f arg =
  try f arg
  with Unix.Unix_error (Unix.EINTR, _, _) -> restart f arg

(* The approximate frame rate (will actually be lower if we take non-trivial
   time to handle events) *)
let frame_rate = 30.0

(* Our basic event loop just calls read_event, which fires the appropriate
   events, then synchronizes the shadow graphics buffer with the screen,
   and then loops again. *)
let rec event_loop () =
  read_event ();
  Graphics.synchronize ();
  restart Thread.delay (1.0 /. frame_rate);
  event_loop ()


(** The command "run_ui x y init" starts up the graphical environment with a
    window size of x by y pixels, sets up the basic events such as the
    keyboard, mouse, etc. (see below), and then invokes the function init as
    an initializer, before entering an event polling loop which fires the
    appropriate event handlers whenever an action occurs. *)
let run_ui (x:int) (y:int) (init:unit->unit) : unit =
  try
    Graphics.open_graph "" ; Graphics.resize_window x y ;
    Graphics.auto_synchronize false ;
    init () ;
    event_loop ()
  with exn -> (Graphics.close_graph () ; raise exn)


