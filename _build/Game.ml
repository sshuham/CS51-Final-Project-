open Core.Std
open Graphics 
open Draw
    
let circle_radius = 35
let piece_height = 100
let piece_width = 100
let infty = 30000000
let minimax_depth = 6
let width = 7
let height = 6

type board = int array array 
type c4Move = int * int 

class type drawable =
object
  method private print_piece : int -> int -> Graphics.color -> string-> unit
  method private grid : unit->unit
end 

class draw : drawable = 
object 
  method private print_piece x y bg t= ignore(Graphics.set_color bg; Graphics.fill_circle x y circle_radius);
               Graphics.set_color Graphics.white; Graphics.moveto x y; Graphics.draw_string t
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
    
let not_alter_next_board (b : board) (move : c4Move) : board =
  let b' = Array.make_matrix 7 6 0 in 
  for i = 0 to 6 do
    for j = 0 to 5 do
      let vl = b.(i).(j) in 
      b'.(i).(j) <- vl
    done 
  done;

    if allowed b' move then
      let count = ref 0 in
      let (m,p) = move in 
      for i = 0 to (height-1) do
  if b'.(m).(i) = 0 then count := !count + 1 done;
      match p with 
      | 1 -> (b'.(m).(6-(!count)) <- 1; b')
      | 2 -> (b'.(m).(6-(!count)) <- 2; b')
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
  method! player_name = "Player " ^ (string_of_int i) 
  method! next_move _ = let s = Graphics.wait_next_event [Graphics.Button_down] in
        let x = s.Graphics.mouse_x in 
        if 0<x && x<100 && s.Graphics.button then (0,i)
        else if 100<x && x<200 && s.Graphics.button then (1,i)
        else if 200<x && x<300 && s.Graphics.button then (2,i)
        else if 300<x && x<400 && s.Graphics.button then (3,i)
        else if 400<x && x<500 && s.Graphics.button then (4,i)
        else if 500<x && x<600 && s.Graphics.button then (5,i)
        else if 600<x && x<700 && s.Graphics.button then (6,i)
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
    if i <= 3 
    then v := !v + check_direction i j (1,0)
    else () ;
    if j <= 2
    then v := !v + check_direction i j (0,1)
    else () ;    
    if i <= 3 && j <= 2
    then v := !v + check_direction i j (1,1)
    else () ;
    if i <= 3 && j >= 3
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
    if i >= 3 && j <= 2 
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
    
let board_full (b : board) = 
  let v = ref true in
  for i = 0 to width - 1 do 
    if allowed b (i, 1) 
    then v := false 
    else ()
  done; !v  

let board_almost_full (b : board) = 
  let f (a : int array) (x : int) : int = 
    Array.fold_right ~f:(fun t n -> if t = 0 then n + 1 else n) a ~init:0 
  in 
  let vl = Array.fold_right ~f:f b ~init:0 in
  vl = 1


class minimaxPlayer (b : board) (nump : int) : player =
object 
  inherit c4Player b nump 
  val num = nump
  
  method! player_name = "Computer " ^ (string_of_int num)
  method! next_move (bd : board) =

  (* 
   * INITIAL MINIMAX
   *)
  
  (*
   let rec minimax (b : board) (d : int) (num_player : int) : (int * int) = 
    if d = 0 || board_full b || board_almost_full b 
    then 
      if num = num_player
      then 
        (let v = ref (-infty) in 
        let index = ref 0 in 
        for i = 0 to 6 do 
          let mv = (i , num_player) in 
          if allowed b mv 
          then  
            (let vl = estimate_value (not_alter_next_board b mv) num_player in
            if vl > !v 
            then (v := vl; index := i)
            else ())
          else ()
        done ; (!index, !v))
      else 
        (let v = ref infty in 
        let index = ref 0 in 
        for i = 0 to 6 do 
          let mv = (i , num_player) in 
          if allowed b mv 
          then  
            (let vl = estimate_value (not_alter_next_board b mv) num_player in
            if vl < !v 
            then (v := vl; index := i)
            else ())
          else ()
        done ; (!index, !v))


    else
      if num = num_player
      then 
        (let bestValue = ref (-infty) in
        let indexValue = ref 0 in
        for i = 0 to 6 do
          let mv = (i,num_player) in
          if allowed b mv
          then 
            (let (_ , vl) = minimax (not_alter_next_board b mv ) (d - 1) (3 - num_player) in 
            if vl > !bestValue
            then (bestValue := vl; indexValue := i)
            else () )
          else ()
        done;
        (!indexValue, !bestValue))
      else
        (let bestValue = ref infty in
        let indexValue = ref 0 in
        for i = 0 to 6 do
          let mv = (i,num_player) in
          if allowed b mv
          then 
            (let (_, vl) = minimax (not_alter_next_board b mv) (d - 1) (3 - num_player) in 
            if vl < !bestValue
            then (bestValue := vl; indexValue := i)
            else () )
          else ()
        done;
        (!indexValue, !bestValue))
  in
  let (mv, _) = minimax bd minimax_depth num in
  (mv,num) 
*)

  (* 
   * SOME OTHER MINIMAX STUFF THAT DOES NOT WORK
   *)


  (*let rec minimax (b : board) (d : int) (num_player : int) : int = 
    if d = 0 || estimate_value b num_player = infty || estimate_value b num_player = - infty || board_full b
    then 
      estimate_value b num_player 
    else
      if num_player = num
      then 
        (let bestValue = ref (-infty - 1) in
        for i = 0 to 6 do
          let mv = (i,num_player) in
          if allowed b mv
          then 
            (let vl = minimax (not_alter_next_board b mv) (d - 1) (3 - num_player) in 
            if vl > !bestValue
            then (bestValue := vl)
            else () )
          else ()
        done;
         !bestValue)
      else
        (let bestValue = ref (infty + 1) in
        for i = 0 to 6 do
          let mv = (i,num_player) in
          if allowed b mv
          then 
            (let vl = minimax (not_alter_next_board b mv) (d - 1) (3 - num_player) in 
            if vl < !bestValue
            then (bestValue := vl)
            else () )
          else ()
        done;
        !bestValue)
  in

  let bestValue = ref (-infty) in 
  let finindexValue = ref 1 in 

  for i = 0 to width - 1 do
    if allowed b (i, num) 
    then 
      let vl = minimax b minimax_depth (3 - num) in
      if !bestValue < vl
      then (bestValue := vl ; finindexValue := i) 
      else ()
    else ()
  done ; (!finindexValue, num)*)

  (* 
   * ALPHA-BETA PRUNING 
   *)


  let rec alphabeta (brd: board) (d : int) (num_player : int) (alpha : int) (beta : int) : (int * int) = 
    let a = ref alpha in (* initialize alpha as best already explored option for maximizer *)
    let b = ref beta in (* initialize beta as best already explored option for minimizer *)
    if d = 0 || board_full brd (*|| board_almost_full brd*)
    then 
      if num = num_player
      then 
        (let v = ref (-infty) in
        let index = ref 0 in 
        for i = 0 to 6 do 
          let mv = (i , num_player) in 
          if allowed brd mv 
          then  
            (let vl = estimate_value (not_alter_next_board brd mv) num_player in
            if vl > !v (* if value of potential move is greater than current option *)
            then (v := vl; index := i)
            else ())
          else ()
        done ; (!index, !v)) (*replicate above ab-pruning below *)
      else 
        (let v = ref infty in 
        let index = ref 0 in 
        for i = 0 to 6 do 
          let mv = (i , num_player) in 
          if allowed brd mv 
          then  
            (let vl = estimate_value (not_alter_next_board brd mv) num_player in
            if vl < !v 
            then (v := vl; index := i)
            else ())
          else ()
        done ; (!index, !v))


    else
      if num = num_player
      then 
        (let bestValue = ref (-infty) in 
        let indexValue = ref 0 in
        let break = ref true in
        let i = ref 0 in 

        while (!i <= 6 && !break) do
          (let mv = (!i,num_player) in
          if allowed brd mv
          then 
            (let (_ , vl) = alphabeta (not_alter_next_board brd mv) (d - 1) (3 - num_player) !a !b in 
            (*if vl > !bestValue
            then
              (bestValue := vl; indexValue := !i;*)
              if vl >= !a && vl > !bestValue(* if value of potential move is greater than alpha *)
              then (a := vl; indexValue := !i; bestValue := vl;
                if !b < !a
                then break := false
                else ()) (* then sets value of a to the value of node *)
              else () )
          else (); i := !i + 1)
        done;
        (!(indexValue), !a))


      else
        (let bestValue = ref infty in
        let indexValue = ref 0 in
        let break = ref true in
        let i = ref 0 in 

        while (!i <= 6 && !break) do
          (let mv = (!i,num_player) in
          if allowed brd mv
          then 
            (let (_ , vl) = alphabeta (not_alter_next_board brd mv) (d - 1) (3 - num_player) !a !b in 
            (*if vl < !bestValue
            then
              (bestValue := vl; indexValue := !i; *)
              if vl <= !b && vl < !bestValue (* if value of potential move is greater than alpha *)
              then (b := vl; indexValue := !i; bestValue := vl;
                if !b < !a
                then break := false
                else ()) (* then sets value of a to the value of node *)
              else ()) (* if value of potential move is less than alpha, it should break here *)
          else (); i := !i + 1)
        done;
        (!(indexValue), !b))
  in
  let (mv, _) = alphabeta bd minimax_depth num (-infty) infty in
  (mv,num)
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

  method print_board : unit= 
    Graphics.clear_graph();
    self#grid();
    match board with 
    | [||] -> failwith "Invalid Board"
    | _ -> for j = 0 to (height-1) do
     (for i = 0 to (width-1) do 
       match board.(i).(j) with
       | 0 -> self#print_piece ((i * 100)+50) ((j * 100)+50) Graphics.black ""
       | 1 -> self#print_piece ((i * 100)+50) ((j * 100)+50) Graphics.red "P1"
       | 2 -> self#print_piece ((i * 100)+50) ((j * 100)+50) Graphics.blue "P2"
       | _ -> failwith "Invalid Board" 
     done)
     done
       
  method switch_player : unit =
    if current_player = player1 then current_player <- player2 else current_player <- player1

  method play() : unit =
    self#print_board;
    self#grid(); 
    Graphics.set_color Graphics.black;
    Graphics.fill_rect 0 605 700 105;
    if (estimate_value board 1 = infty) || (estimate_value board 1 = (infty * -1))
    then 
      let name = current_player#player_name in 
      print_endline ("Congratulations " ^ name ^ ", you win!\n"); 
      exit 1
    else 
      (self#switch_player;
       Graphics.set_color Graphics.white;
       Graphics.moveto 250 650;
       Graphics.draw_string (current_player#player_name ^ ", it's your turn.");
       let move = current_player#next_move (self#get_board) in
       if allowed board move  
       then 
   ((ignore (next_board (self#get_board) (move))); 
    self#grid(); 
    self#print_board;
    self#play())
       else self#switch_player;
       self#play())
  
end
