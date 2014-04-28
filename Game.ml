open Core.Std
open Graphics 
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
end

class c4Player (b : board) : player = 
object
  val mutable move = true 
  method player_name = "Bob"
  method next_move _ = new c4Move 0
end

class humanPlayer (b : board) : player =  
object
  method player_name = "Bob"
  method next_move b = new c4Move 0
end

class minimaxPlayer (b : board) : player =
object 
  inherit c4Player as super

  method! next_move (bd : board) =
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


class type game =
object
  inherit drawable 
  method move_of_string : string -> move
  method current_player : player
  method string_of_move : move -> string 
  method print_board : board -> unit
  method allowed : board -> move -> bool
  method next_board : board -> move -> board
end

class c4Game (player1 : c4Player) (player2 : c4Player) : game  =
object (self)

  inherit draw
  
  val mutable current_player = player1 

  val mutable board : c4Board = new c4Board

  method move_of_string (column : string) : move =
    new c4Move (int_of_string column)
  
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
		   | 2 -> self#print_piece ((j*10), (i*10)) Graphics.blue "P2" 
		   | _ -> failwith "Invalid Board" 
		 done 
	   done

  method allowed (b : c4Board) (m : c4Move) : bool =
    let move = m#get_move in
    let board = b#get_board in
    move > 0 || move <= b#get_width 
    || Array.fold_right ~f: (fun x _ -> phys_equal x 0) ~init:true board.(move)  
   
  method switch_player : unit =
    if current_player = player1 then player2 else player1 
    
  method next_board (m : c4Move) : board =
    if self#allowed b m then
      let count = ref 0 in
      let move = m#get_move in
      for i = 0 to (board#get_height-1) do
	if phys_equal board.(move).(i) 0 then count := !count + 1 done;
      match current_player with 
      | player1 -> b#get_board.(move).(!count-1) <- 1
      | player2 -> b#get_board.(move).(!count-1) <- 2
    else failwith "Illegal Move"

  method is_won (b : c4Board) : bool = 
    for i = 0 to (b#get_height - 1) do 
      for j = 0 to (b#get_width - 1) do 
	phys_equal b.(i).(j) b.(i).(j) && phys_equal b.(i).(j) b.(i+1).(j)  
	&& phys_equal b.(i).(j) b.(i+2).(j)  && phys_equal b.(i).(j) b.(i+3).(j) 
	|| phys_equal b.(i).(j) b.(i).(j) && phys_equal b.(i).(j) b.(i).(j+1)  
	   && phys_equal b.(i).(j) b.(i).(j+2)  && phys_equal b.(i).(j) b.(i).(j+3) 
	|| phys_equal b.(i).(j) b.(i).(j) && phys_equal b.(i).(j) b.(i+1).(j+1)  
	   && phys_equal b.(i).(j) b.(i+2).(j+2)  && phys_equal b.(i).(j) b.(i+3).(j+3) 
	|| phys_equal b.(i).(j) b.(i).(j) && phys_equal b.(i).(j) b.(i+1).(j-1)  
	   && phys_equal b.(i).(j) b.(i+2).(j-2)  && phys_equal b.(i).(j) b.(i+3).(j-3) 


  method play = 

(*while not won, ask player for move, get move, print board, check if won, switch players.....then call play() in main function  
end
