open Core.Std

module type GAME =
sig
  type board
  type move
  type player = | Player1 | Player2
  val initial_board : int -> int -> player -> board
  val move_of_string : string -> move
  val new_board : board -> move -> board
  val allowed : move -> bool
  val allowed_moves : board -> move list
  val current_player : board -> player
  val is_won : board -> bool
end

module Connect_Four =
struct
  type move = int
  type player = | Player1 | Player2
  type board = (int array array) * player
  let initial_board (width : int) (height : int) (player : player) : board =
    (Array.create ~len: height (Array.create ~len: width 0), player)
end
