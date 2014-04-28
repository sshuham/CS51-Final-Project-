open Core.Std
open Graphicevents

(********************)
(***** UI State *****)
(********************)

let mouse_state = ref false
let mouse_pos = ref (0,0)
let board_width = 70
let board_height = 60 

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
;;
