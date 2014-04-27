(*
CS51 Final Project - Connect 4 
Partner 1: Simon Shuham
Partner 2: Winston Huang
Partner 3: Octav Dragoi
Partner 4: Alan Xie 
 *)

open Core.Std
open Graphicevents


(* Initializer functions *)
let hh_initializer () : unit =
  ignore (new Game.game (new Game.humanPlayer) (new Game.humanPlayer)) 

let hc_initializer () : unit =
  ignore (new Game.game (new Game.humanPlayer) (new Game.minimaxPlayer)) 

let ch_initializer () : unit =
  ignore (new Game.game (new Game.minimaxPlayer) (new Game.humanPlayer)) 

let cc_initializer () : unit =
  ignore (new Game.game (new Game.minimaxPlayer) (new Game.minimaxPlayer)) 


(* Parse command-line arguments. Returns the appropriate initialization
  function to run and the current part. *)
let parse_args () : (unit -> unit) =
  let usage () = Printf.printf "usage: %s argument\n" Sys.argv.(0); exit 1 in
  if Array.length Sys.argv <> 2 then usage ();
  match Sys.argv.(1) with
  | "human human" | "Human human" | "human Human" | "Human Human"-> hh_initializer
  | "human computer" | "Human computer" | "human Computer" | "Human Computer" -> hc_initializer
  | "computer human" | "Computer human" | "computer Human" | "Computer Human" -> ch_initializer
  | "computer computer" | "Computer computer" | "computer Computer" | "Computer Computer" -> ch_initializer
  | _ -> usage ()

let board_width = 7
let board_height = 6

let run () : unit =
  let initialize = parse_args () in
  Interface.run_ui board_width board_height initialize 
;;

run () ;;
