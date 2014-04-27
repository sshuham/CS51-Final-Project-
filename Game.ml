open Core.Std
open Graphics 
<<<<<<< HEAD:Game.ml
open Graphicevents
=======
open Event
>>>>>>> 27e74a2645f9d86e70783d8f35d8e96470174920:game.ml
open Draw
    
let circle_height = 7
let circle_width = 7
let piece_height = 10
let piece_width = 10 
let infty = 10000000
let minimax_depth = 5

class type drawable =
object
  method private draw_rect : int*int -> unit
  method private draw_circle : int*int -> Graphics.color -> string -> unit
  method private print_piece : int*int -> Graphics.color -> string -> unit 
  (*method private grid : int -> int -> int -> unit*)
end 

class draw : drawable = 
object (self)
  method private draw_circle pos bg text = Draw.circle pos circle_width circle_height bg Graphics.black text
  method private draw_rect pos = Draw.rect pos piece_width piece_height Graphics.white 
  method private print_piece pos bg text = self#draw_rect pos; let (x,y) = pos in self#draw_circle (x+5, y+5) bg text
  (*method private grid height length bars = ????*)
end 


class type board =
object
  val mutable board : int array array
  method get_height : int
  method get_width : int
  method get_board : int array array
end

class c4Board : board =
object 
<<<<<<< HEAD:Game.ml
  val mutable board = Array.make_matrix ~dimx:7 ~dimy:6 0
=======
  val mutable board = Array.make_matrix 7 6 0
>>>>>>> 27e74a2645f9d86e70783d8f35d8e96470174920:game.ml
  method get_height = 6
  method get_width = 7
  method get_board = board
end

class type move =
object
  method get_move : int
end

class c4Move num : move =
object
  method get_move = num
end

class type player =
object
  method next_move : board -> move
  method player_name : string
  method is_move : bool
  method set_move : bool -> unit
end

<<<<<<< HEAD:Game.ml
class c4Player : player = 
=======



class c4Player (b : board) : player = 
>>>>>>> 27e74a2645f9d86e70783d8f35d8e96470174920:game.ml
object
  val mutable move = true

  method is_move = move
<<<<<<< HEAD:Game.ml
  method set_move x = move <- x 
  method player_name = "Bob"
  method next_move _ = new c4Move 0
end

(*class humanPlayer (b : board) : player = 
=======
  method set_move b = move <- b 
  method player_name = "Bob"

  method next_move b = new c4Move 0
end

class humanPlayer (b : board) : player = 
>>>>>>> 27e74a2645f9d86e70783d8f35d8e96470174920:game.ml
object
end 

class minimaxPlayer (b : board) : player =
object 
  inherit c4Player as super

<<<<<<< HEAD:Game.ml

  !method next_move board =
     let minimax (b : board) (d : int) (max_player : bool) = 
       if d = 0 || 

end  *)
=======
  !method next_move (bd : board) =
    let test_game = new c4Game (new c4Player board) (new c4Player board) in 
      let minimax (b : board) (d : int) (max_player : bool) : (int * int) = 
        if d = 0 || test_game#is_won board != None
        then 0 (* insert heuristic function here *)
        else
          if max_player
          then 
            (let bestValue = ref (-infty) in
            let indexValue = ref 0 in
            for i = 0 to board#get_width - 1 do
              let mv = new c4Move i in
              if test_game#allowed b mv
              then 
                (let (_ , val) = minimax (test_game#next_board b mv) (d - 1) false in 
                if val > !bestValue
                then (bestValue := val; indexValue := i)
                else () )
              else ();
            (!indexValue, !bestValue))
          else
            (let bestValue = ref infty in
            let indexValue = ref 0 in
            for i = 0 to board#get_width - 1 do
              let mv = new c4Move i in
              if test_game#allowed b mv
              then 
                (let (_, val) = minimax (test_game#next_board b mv) (d - 1) false in 
                if val < !bestValue
                then (bestValue := val; indexValue := i)
                else () )
              else ();
            (!indexValue, !bestValue)
      in
      let (mv, _) = minimax b minimax_depth true in
      new c4Move mv


end  
>>>>>>> 27e74a2645f9d86e70783d8f35d8e96470174920:game.ml



class type game =
object
  inherit drawable 
  method initial_board : player -> board * player
  method move_of_string : string -> move
  method current_player : player
  method string_of_move : move -> string 
  method print_board : board -> unit
  method allowed : board -> move -> bool
<<<<<<< HEAD:Game.ml
  (*method change_player_to_move : player -> player -> unit*)
  method next_board : board -> move -> unit
  (*method is_won : board -> bool*)
=======
  method change_player_to_move : player -> player -> unit
  method next_board : board -> move -> unit
  method is_won : board -> bool
>>>>>>> 27e74a2645f9d86e70783d8f35d8e96470174920:game.ml
end

class c4Game (player1 : c4Player) (player2 : c4Player) : game  =
object (self)

  inherit draw
  
  val mutable board : c4Board = new c4Board

  method initial_board : c4Board * c4Player =
    (board, player1)

  method move_of_string (column : string) : move =
    new c4Move (int_of_string column)

  method current_player : c4Player =
    if player1#is_move then player1 else player2
  
  method string_of_move (m : c4Move) : string = 
    string_of_int m#get_move

  method print_board (b : c4Board) :  unit = 
    match b#get_board with 
    | [||] -> failwith "Invalid Board"
    | _ -> for i = 0 to (b#get_width-1) do
	     let bi = b#get_board.(i) in
		 for j = 0 to (b#get_height-1) do 
		   match bi.(j) with
		   | 0 -> self#print_piece ((j*10), (i*10)) Graphics.white ""
		   | 1 -> self#print_piece ((j*10), (i*10)) Graphics.red "P1"
<<<<<<< HEAD:Game.ml
		   | 2 -> self#print_piece ((j*10), (i*10)) Graphics.blue "P2" 
		   | _ -> failwith "Invalid Board" 
		 done 
	   done 


=======
		   | 2 -> self#print_piece ((j*10), (i*10)) Graphics.blue "P2" done done
		  
	     
>>>>>>> 27e74a2645f9d86e70783d8f35d8e96470174920:game.ml
  method allowed (b : c4Board) (m : c4Move) : bool =
    let move = m#get_move in
    let board = b#get_board in
    move > 0 || move <= b#get_width 
    || Array.fold_right ~f: (fun x _ -> phys_equal x 0) ~init:true board.(move)

<<<<<<< HEAD:Game.ml
  (*method change_player_to_move (p1 : c4Player) (p2: c4Player) : unit =
    p1#is_move = (not p1#is_move); p2#is_move = (not p2#is_move)*)
   
=======
  method change_player_to_move (p1 : c4Player) (p2: c4Player) : unit =
    p1#is_move = (not p1#is_move); p2#is_move = (not p2#is_move); ()
>>>>>>> 27e74a2645f9d86e70783d8f35d8e96470174920:game.ml

  method next_board (b : c4Board) (m : c4Move) : unit =
    if self#allowed b m then
      let count = ref 0 in
      let move = m#get_move in
      for i = 0 to (b#get_height-1) do
	if phys_equal b#get_board.(move).(i) 0 then count := !count + 1 done;
<<<<<<< HEAD:Game.ml
      match self#current_player with 
      | player1 -> let new_board = b#get_board.(move).(!count-1) <- 1 in print_board new_board
      | player2 -> let new_board = b#get_board.(move).(!count-1) <- 2 in print_board new_board 
=======
      b#get_board.(move).(!count-1) <- 1; ()
>>>>>>> 27e74a2645f9d86e70783d8f35d8e96470174920:game.ml
    else failwith "Illegal Move"


  (* when you implement is_won, please make sure to return the player who won the game, 
<<<<<<< HEAD:Game.ml
   * so it's sth like "player option"*)

  (*method is_won (b : c4Board) : player option =
    let board = b#get_board in
    let rec check_row (board_array : Array) : bool =*)
=======
   * so it's sth like "player option" *)

  method is_won (b : c4Board) : player option = None
>>>>>>> 27e74a2645f9d86e70783d8f35d8e96470174920:game.ml
end