open Core.Std

class type board =

class type player = 

class C4Player : player = 

class type move = 

class type game =
object
  method initial_board : int -> int -> player -> board
  method move_of_string : string -> move
  method current_player : board -> player
  method string_of_move : move -> string 
  method print_board : board -> unit 
  method allowed : move -> bool
  method allowed_moves : board -> move list
  method next_board : board -> move -> board 
  method is_won : board -> bool
end

class connect_Four player1 player2 : game  =
object
  (*type move
  val mutable board (* move to class *)*)
  let p1 = new C4Player (* move to class *)

  let height = 6

  let width = 7

  method initial_board (player : player) : board =
    (Array.make_matrix height width 0, player)

  method move_of_string (column : string) : int =
    int_of_string column
  
  method string_of_move (m : move) : string = 
    string_of_int m

  method print_board (b : board) :  unit = 
    match b with 
    | [] -> failwith "Invalid Board" 
    | _ -> for i = 0 to height do
	     let bi = b.(i) in
	     for j = 0 to width do 
	       print_int bi.(j) in print_newline

    
end
