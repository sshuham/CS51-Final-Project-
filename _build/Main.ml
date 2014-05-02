(*
CS51 Final Project - Connect 4 
Partner 1: Simon Shuham
Partner 2: Winston Huang
Partner 3: Octav Dragoi
Partner 4: Alan Xie 
 *)

open Core.Std
open Graphicevents
open Game 

let board_height = 700

let board_width = 700

let board = Array.make_matrix ~dimx:7 ~dimy:6 0 

let current_game = ref (new c4Game (new humanPlayer board 1) (new humanPlayer board 2))

(* Initializer functions *)
let hh_initializer () : unit =
  ignore (current_game := (new c4Game (new humanPlayer board 1) (new humanPlayer board 2)))

let hc_initializer () : unit =
  ignore (current_game := (new c4Game (new humanPlayer board 1) (new minimaxPlayer board 2)))

let ch_initializer () : unit =
  ignore (current_game := (new c4Game (new minimaxPlayer board 1) (new humanPlayer board 2)))

let cc_initializer () : unit =
  ignore (current_game := (new c4Game (new minimaxPlayer board 1) (new minimaxPlayer board 2)))


(* Parse command-line arguments. Returns the appropriate initialization
  function to run and the current part. *)
let parse_args () : (unit -> unit) =
  let usage () = Printf.printf "usage: %s argument\n" Sys.argv.(0); exit 1 in
  if Array.length Sys.argv <> 2 then usage ();
  match Sys.argv.(1) with
  | "hh"-> hh_initializer
  | "hc" -> hc_initializer
  | "ch" -> ch_initializer
  | "cc" -> cc_initializer
  | _ -> usage ()

let run () : unit =
  let initialize = parse_args () in
  Interface.run_ui board_width board_height initialize;
  let game = !current_game in 
  game#play(); 
;;

run () ;;

