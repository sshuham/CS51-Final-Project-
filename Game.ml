open Core.Std
open Graphics 
open Draw
    
let circle_height = 7
let circle_width = 7
let piece_height = 10
let piece_width = 10 
let infty = 10000000
let minimax_depth = 5
let width = 7
let height = 6

type board = int array array 
type c4Move = int * int 

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

let allowed (b : board) (move : c4Move) : bool =
  let (m,_) = move in 
    m > 0 || m <= width
    || Array.fold_right ~f: (fun x _ -> phys_equal x 0) ~init:true b.(m) 

let next_board (b : board) (move : c4Move) : board =
    if allowed b move then
      let count = ref 0 in
      let (m,p) = move in 
      for i = 0 to (height-1) do
	if phys_equal b.(m).(i) 0 then count := !count + 1 done;
      match p with 
      | 1 -> (b.(m).(!count-1) <- 1; b)
      | 2 -> (b.(m).(!count-1) <- 2; b)
      | _ -> failwith "Not legal"
    else failwith "Illegal Move"

class type player =
object
  method next_move : board -> int -> c4Move
  method player_name : string
end

class c4Player (_ : board): player = 
object
  method player_name = "Bob"
  method next_move _ _ = (0,0)
end

class humanPlayer (b : board) : player = 
object
  inherit c4Player b
  method! player_name = "Human" 
  method! next_move _ _ = let (x,_) = Graphics.mouse_pos() in 
			  if 0<x && x<10 && Graphics.button_down() then (1,0) 
			  else if 10<x && x<20 && Graphics.button_down() then (2,0)
			  else if 20<x && x<30 && Graphics.button_down() then (3,0)  
			  else if 30<x && x<40 && Graphics.button_down() then (4,0) 
			  else if 40<x && x<50 && Graphics.button_down() then (5,0) 
			  else if 50<x && x<60 && Graphics.button_down() then (6,0)
			  else if 60<x && x<70 && Graphics.button_down() then (7,0)
			  else (0,0)			  
end 

class minimaxPlayer (b : board) : player =
object 
  inherit c4Player b 

  method! player_name = "Computer" 
  method! next_move (_ : board) (num : int) =
      let rec minimax (b : board) (d : int) (max_player : bool) (num_player : int) : (int * int) = 
        if d = 0 
        then (0,0) (* insert heuristic function here *)
        else
          if max_player
          then 
            (let bestValue = ref (-infty) in
            let indexValue = ref 0 in
            for i = 0 to 7 - 1 do
              let mv = (i,num_player) in
              if allowed b mv
              then 
                (let (_ , vl) = minimax (next_board b mv ) (d - 1) false (3 - num_player) in 
                if vl > !bestValue
                then (bestValue := vl; indexValue := i)
                else () )
              else ()
            done;
            (!indexValue, !bestValue))
          else
            (let bestValue = ref infty in
            let indexValue = ref 0 in
            for i = 0 to 7 - 1 do
              let mv = (i,num_player) in
              if allowed b mv
              then 
                (let (_, vl) = minimax (next_board b mv) (d - 1) false (3 - num_player) in 
                if vl < !bestValue
                then (bestValue := vl; indexValue := i)
                else () )
              else ()
            done;
            (!indexValue, !bestValue))
      in
      let (mv, _) = minimax b minimax_depth true num in
      (mv,num)
end  


class type game =
object
  inherit drawable 
  method print_board : unit
  method switch_player : unit 	
  method is_won : bool 			  
end

class c4Game (player1 : c4Player) (player2 : c4Player) : game  =
object (self)

  inherit draw
  
  val mutable current_player = player1 

  val mutable board : board = Array.make_matrix ~dimx:width ~dimy:height 0 

  method print_board = 
    match board with 
    | [||] -> failwith "Invalid Board"
    | _ -> for i = 0 to (width-1) do
	     let bi = board.(i) in
		 for j = 0 to (height-1) do 
		   match bi.(j) with
		   | 0 -> self#print_piece ((j*10), (i*10)) Graphics.white ""
		   | 1 -> self#print_piece ((j*10), (i*10)) Graphics.red "P1"
		   | 2 -> self#print_piece ((j*10), (i*10)) Graphics.blue "P2" 
		   | _ -> failwith "Invalid Board" 
		 done 
	   done 
   
  method switch_player : unit =
    if current_player = player1 then current_player <- player2 else current_player <- player1

  method is_won : bool = 
    let v = ref false in
    for i = 0 to (height - 1) do 
      for j = 0 to (width - 1) do 
	if not(phys_equal board.(i).(j) 0) then 
		v := !v || (phys_equal board.(i).(j) board.(i).(j) && phys_equal board.(i).(j) board.(i+1).(j)  
		&& phys_equal board.(i).(j) board.(i+2).(j)  && phys_equal board.(i).(j) board.(i+3).(j) 
		|| phys_equal board.(i).(j) board.(i).(j) && phys_equal board.(i).(j) board.(i).(j+1)  
		   && phys_equal board.(i).(j) board.(i).(j+2)  && phys_equal board.(i).(j) board.(i).(j+3) 
		|| phys_equal board.(i).(j) board.(i).(j) && phys_equal board.(i).(j) board.(i+1).(j+1)  
		   && phys_equal board.(i).(j) board.(i+2).(j+2)  && phys_equal board.(i).(j) board.(i+3).(j+3) 
		|| phys_equal board.(i).(j) board.(i).(j) && phys_equal board.(i).(j) board.(i+1).(j-1)  
		   && phys_equal board.(i).(j) board.(i+2).(j-2)  && phys_equal board.(i).(j) board.(i+3).(j-3)) 
	       done 
	       done; 
	       !v 

(*while not won, ask player for move, get move, print board, check if won, switch players.....then call play() in main function *)
end
