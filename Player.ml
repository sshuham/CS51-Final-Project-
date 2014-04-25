open Core.Std
open Game 

class type player = 
object
 
  method next_move : board -> move 

  method player_name : board -> string

end;; 
