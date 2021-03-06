module G = Geometry

exception Game_over

type color =
  | Black
  | Red
  | Green
  | Blue

let string_of_color = function
  | Black -> "."
  | Red -> "r"
  | Green -> "g"
  | Blue -> "b"

type direction =
  | Left
  | Right

let string_of_direction = function
  | Left -> "Left"
  | Right -> "Right"
      
type 'a bc =
  | Label of 'a
  | Move
  | Rotate of direction
  | Color of color
  | Call of 'a
  | Return

let string_of_bc string_of_a = function
  | Label l -> "Label " ^ string_of_a l
  | Move -> "Move"
  | Rotate d -> "Rotate " ^ string_of_direction d
  | Color c -> "Color " ^ string_of_color c
  | Call f -> "Call " ^ string_of_a f
  | Return -> "Return"

type square = {
  star: bool;
  color: color
}
	       
type puzzle = {
  table: square array array;
  lines: int;
  columns: int;
  start_position: G.point;
  start_orientation: G.vector
}

type state = {
  position: G.point;
  orientation: G.vector;
  board: puzzle;
  stars: int
}

let print_puzzle {table; lines; columns} =
  let open Printf in
  let open String in
  for i = 0 to lines - 1 do
    for j = 0 to columns - 1 do
      let {star = s; color = c} = table.(i).(j) in
      let cs = string_of_color c in
      printf "%s" (if (s: bool) then uppercase cs else cs)
    done;
    print_newline ()
  done

let is_in {lines; columns} (G.P (x,y)) =
  x >= 0 && x < columns && y >= 0 && y < lines

let next p o = G.move p o

let move p o b =
  let p' = next p o in
  if p' |> is_in b then
    p'
  else
    raise Game_over
    
let color p ({table = b} as puzzle) c =
  let open Geometry in
  let (P (x,y)) = p in
  let square = b.(y).(x) in
  let square' = {square with color = c} in
  b.(y).(x) <- square';
  {puzzle with table = b}
	       
let step ({position = p;			 
	   orientation = o;
	   board = b;
	   stars = s} as state) = function
  | Move ->
     let p' = move p o b in
     {state with position = p'}
  | Rotate Left ->
     let o' = G.rotate_left o in
     {state with orientation = o'}
  | Rotate Right ->
     let o' = G.rotate_right o in
     {state with orientation = o'}
  | Color c ->
     let b' = color p b c in
     {state with board = b'}
     
