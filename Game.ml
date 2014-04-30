open Core.Std
open Graphics 
open Draw
    
let circle_height = 70
let circle_width = 70
let piece_height = 100
let piece_width = 100
let infty = 10000000
let minimax_depth = 5
let width = 7
let height = 6

type board = int array array 
type c4Move = int * int 

class type drawable =
object
  (*method private draw_rect : int*int -> unit
  method private draw_circle : int*int -> Graphics.color -> string -> unit*)
  method private print_piece : int*int -> Graphics.color -> string -> unit 
  method private grid : unit->unit
end 

class draw : drawable = 
object (self)
  method private print_piece pos bg text = Draw.circle pos circle_width circle_height bg Graphics.black text
  (*method private draw_rect pos = Draw.rect pos piece_width piece_height Graphics.yellow 
  method private print_piece pos bg text = self#draw_rect pos; let (x,y) = pos in self#draw_circle (x+50, y+50) bg text*)
  method private grid() = for i=0 to height do
			  for j=0 to width do
			    (Graphics.moveto (j*100) 0; 
			    Graphics.lineto (j*100) (height*100);
			    Graphics.moveto 0 (i*100);
			    Graphics.lineto (width*100) (i*100))
			  done 
			done
end 

let allowed (b : board) (move : c4Move) : bool =
 let (m,_) = move in 
    m >= 0 && m < width
    && b.(m).(5) = 0

let next_board (b : board) (move : c4Move) : board =
    if allowed b move then
      let count = ref 0 in
      let (m,p) = move in 
      for i = 0 to (height-1) do
	if b.(m).(i) = 0 then count := !count + 1 done;
      match p with 
      | 1 -> (b.(m).(6-(!count)) <- 1; b)
      | 2 -> (b.(m).(6-(!count)) <- 2; b)
      | _ -> failwith "Not legal"
    else failwith "Illegal Move"

class type player =
object
  method next_move : board -> c4Move
  method player_name : string
end

class c4Player (_ : board) (_ : int) : player = 
object
  method player_name = "Bob"
  method next_move _ = (0,0)
end

class humanPlayer (b : board) (i : int) : player = 
object (self)
  inherit c4Player b i 
  method! player_name = "Human" 
  (*method! next_move _ = let s = Graphics.wait_next_event [Graphics.Button_down] in
			  let x = s.Graphics.mouse_x in 
			  if 0<x && x<10 && s.Graphics.button then (0,i) 
			  else if 10<x && x<20 && s.Graphics.button then (1,i)
			  else if 20<x && x<30 && s.Graphics.button then (2,i)  
			  else if 30<x && x<40 && s.Graphics.button then (3,i) 
			  else if 40<x && x<50 && s.Graphics.button then (4,i) 
			  else if 50<x && x<60 && s.Graphics.button then (5,i)
			  else if 60<x && x<70 && s.Graphics.button then (6,i)
			  else failwith "you suck"*)

  method! next_move _ = let str = (input_line stdin) in 
			if String.contains str '0'||
			     String.contains str '1' ||
			       String.contains str '2' ||
				 String.contains str '3' ||
				   String.contains str '4' ||
				     String.contains str '5' ||
				       String.contains str '6' then 
			  let move = (int_of_string str, i) in
			  move 
			else self#next_move b
										 
end 

let estimate_value (b : board) (num_player : int) =
  let w1 = 1 in
  let w2 = 10 in
  let w3 = 100 in
  let w4 = infty in

  let check_direction i j (d : int * int) =
    let v = ref 0 in
    let (i', j') = d in

    if b.(i).(j) = b.(i+i').(j+j')
    then 
      if b.(i).(j) = b.(i+2*i').(j+2*j')
      then 
        if b.(i).(j) = b.(i+3*i').(j+3*j')
        then v := !v + w4
        else v := !v + w3
      else 
        if b.(i).(j) = b.(i+3*i').(j+3*j') && b.(i+2*i').(j+2*j') = 0
        then v := !v + w3
        else v := !v + w2
    else 
      if b.(i).(j) = b.(i+2*i').(j+2*j') && b.(i+i').(j+j') = 0
      then 
        if b.(i).(j) = b.(i+3*i').(j+3*j')
        then v := !v + w3
        else v := !v + w2
      else v := !v + w1 ; !v 
  in 
  let check_vertex (i : int) (j : int) : int = 
    let v = ref 0 in
    if i <= 2 
    then v := !v + check_direction i j (1,0)
    else () ;
    if j <= 1
    then v := !v + check_direction i j (0,1)
    else () ;    
    if i <= 2 && j <= 1
    then v := !v + check_direction i j (1,1)
    else () ;
    if j >= 3 && i <= 2
    then v := !v + check_direction i j (1,-1)
    else () ; 
    if i >= 3 
    then v := !v + check_direction i j (-1,0)
    else () ;
    if j >= 3
    then v := !v + check_direction i j (0,-1)
    else () ;    
    if i >= 3 && j >= 3
    then v := !v + check_direction i j (-1,-1)
    else () ;
    if i >= 3 && j <= 1 
    then v := !v + check_direction i j (-1,1)
    else () ; 
    !v 
  in

  let v = ref 0 in

  for i = 0 to 6 do
    for j = 0 to 5 do 
      if b.(i).(j) = 0 
      then ()
      else 
        if b.(i).(j) = num_player
        then v := !v + check_vertex i j
        else v := !v - check_vertex i j 
    done 
  done ;
  if !v > 2 * infty / 3
  then infty
  else
    if !v < - 2 * infty / 3 
    then - infty

    else !v
	  
class minimaxPlayer (b : board) (num : int) : player =
object 
  inherit c4Player b num

  (*method! player_name = "Computer" 
  method! next_move (bd : board) =
      let rec minimax (b : board) (d : int) (max_player : bool) (num_player : int) : (int * int) = 
        if d = 0 || estimate_value b num_player = infty || estimate_value b num_player = - infty
        then 
          if max_player 
          then 
            (let v = ref 0 in 
            let index = ref 0 in 
            for i = 0 to 7 - 1 do 
              let mv = (i , num_player) in 
              if allowed b mv 
              then  
                (let vl = estimate_value (next_board b mv) in
                if vl > !v 
                then (v := vl; index := i)
                else ())
              else ())
            done ;
            (!index, !v)
          else 
            (let v = ref 0 in 
            let index = ref 0 in 
            for i = 0 to 7 - 1 do 
              let mv = (i , num_player) in 
              if allowed b mv 
              then  
                (let vl = estimate_value (next_board b mv) in
                if vl < !v 
                then (v := vl; index := i)
                else ())
              else ())
            done ;


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
                (let (_, vl) = minimax (next_board b mv) (d - 1) true (3 - num_player) in 
                if vl < !bestValue
                then (bestValue := vl; indexValue := i)
                else () )
              else ()
            done;
            (!indexValue, !bestValue))
      in
      let (mv, _) = minimax bd minimax_depth true num in
      (mv,num)*)
end  


class type game =
object
  inherit drawable 
  method get_board : int array array 
  method print_board : unit
  method switch_player : unit 	
  method play : unit -> unit 		  
end

class c4Game (player1 : c4Player) (player2 : c4Player) : game  =
object (self)

  inherit draw
  
  val mutable current_player = player2

  val mutable board : board = Array.make_matrix ~dimx:width ~dimy:height 0 

  method get_board = board 

  (*method print_board : unit= 
    self#grid();
    match board with 
    | [||] -> failwith "Invalid Board"
    | _ -> for i = 0 to (width-1) do
	     let bi = board.(i) in
		 for j = 0 to (height-1) do 
		   match bi.(j) with
		   | 0 -> self#print_piece ((j*100), (i*100)) Graphics.black ""
		   | 1 -> self#print_piece ((j*100), (i*100)) Graphics.red "P1"
		   | 2 -> self#print_piece ((j*100), (i*100)) Graphics.blue "P2" 
		   | _ -> failwith "Invalid Board" 
		 done 
	   done*)

  method print_board : unit = 
    (*self#grid();*)
    match board with 
    | [||] -> failwith "Invalid Board"
    | _ -> for j = 0 to 5 do
	     (for i = 0 to 6 do 
	       Printf.printf "%d" (board.(i).(5-j))
	     done;
	       Printf.printf "\n")
	   done
	     
  method switch_player : unit =
    if current_player = player1 then current_player <- player2 else current_player <- player1

  (*method play () : unit = 
    let rec play_game () : unit= 
      if (self#is_won) then
	(Graphics.clear_graph(); Graphics.draw_string ("Congratulations" ^ current_player#player_name ^ ", you won!");)
      else 
	(self#switch_player;
      Graphics.draw_string (current_player#player_name ^ ": It is your turn.");
      next_board (self#get_board) (current_player#next_move (self#get_board) 0); 
      self#print_board;)
	  play_game () 
    in play_game ()*)
  

  method play() : unit = 
    if (estimate_value board 1 = infty) || (estimate_value board 1 = (infty * -1))
    then Printf.printf "You win \n"; flush stdout
    else 
      (self#switch_player;
       Printf.printf "Make a move between 0 and 6: \n";
       flush stdout;
       let move = current_player#next_move (self#get_board) in
       if allowed board move  
       then 
	 ((ignore (next_board (self#get_board) (move))); 
	  self#print_board;
	  flush stdout; 
	  self#play())
       else self#switch_player;
       self#play())
	(*(Graphics.clear_graph(); Graphics.draw_string ("Congratulations" ^ current_player#player_name ^ ", you won!");)*) 
end
