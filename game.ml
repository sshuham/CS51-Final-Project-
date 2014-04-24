open Core.Std
open Graphics 

class type drawable =
object
  method rect : int -> int -> int -> int -> unit
  method button : rect -> string -> unit
  method circle : int*int -> int -> int -> color -> color -> string -> unit
  method piece : rect -> circle -> unit 
  method grid : int -> int -> int -> unit 
end 

class type board =
object
  val mutable board : int array array
  method get_height : int
  method get_width : int
  method get_board : int array array
end

class c4Board : board =
object (self)
  val mutable board = Array.make_matrix 6 7 0
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
  method player_name : board -> string
  method is_move : bool
end

class c4Player board : player = 
object
  method next_move board = new c4Move 0
  method player_name board = "Bob"
  method is_move = true
end

class type game =
object
  method initial_board : player -> board * player
  method move_of_string : string -> move
  method current_player : player -> player -> player
  method string_of_move : move -> string 
  method print_board : board -> unit 
  method allowed : board -> move -> bool
  (*method next_board : board -> move -> board 
  method is_won : board -> bool*)
end

class c4Game (player1 : c4Player) (player2 : c4Player) : game  =
object (self)

  val mutable board : c4Board = new c4Board

  method initial_board (player : c4Player) : c4Board * c4Player =
    (board, player1)

  method move_of_string (column : string) : move =
    new c4Move (int_of_string column)

  method current_player (player1 : c4Player) (player2 : c4Player) : c4Player =
    if player1#is_move then player1 else player2
  
  method string_of_move (m : c4Move) : string = 
    string_of_int m#get_move

  method print_board (b : c4Board) :  unit = 
    match b#get_board with 
    | [||] -> failwith "Invalid Board"
    | _ -> for i = 0 to (b#get_height - 1) do
	     let bi = b#get_board.(i) in
	     for j = 0 to b#get_width do 
	       (if j = b#get_width then print_newline () else print_int bi.(j)) done done

  method allowed (b : c4Board) (m : c4Move) : bool =
    let move = m#get_move in
    let board = b#get_board in
    move < 0 || move >= b#get_width 
    || Array.fold_right ~f: (fun x y -> phys_equal x 0) ~init:true board.(move)

end
