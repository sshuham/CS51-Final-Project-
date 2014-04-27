open Core.Std
open game 

class type Player = 
object
 
  method next_move : board -> move 

  method player_name : board -> string

end 
