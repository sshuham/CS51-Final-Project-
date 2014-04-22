open Core.Std

module type GAME =
sig
  type move
  type player = | Player1 | Player2
  type board
  val initial_board : int -> int -> player -> board
  val move_of_string : string -> move
  (*val new_board : board -> move -> board
  val allowed : move -> bool
  val allowed_moves : board -> move list
  val current_player : board -> player
  val is_won : board -> bool*)
end

module Connect_Four =
struct
  type move = int
  type player = | Player1 | Player2
  type board = (int array array) * player
  let initial_board (player : player) : board =
    (Array.make_matrix 6 7 0, player)
  let move_of_string (column : string) : int =
    int_of_string column
  let new_board (old_board : board) (new_move : move) : board =
    
end
