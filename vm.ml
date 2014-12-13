module G = Geometry

exception Game_over

type color =
  | Red
  | Green
  | Blue

let string_of_color = function
  | Red -> "r"
  | Green -> "g"
  | Blue -> "b"

type bc =
  | Move
  | RotateLeft
  | RotateRight
  | Color of color

let string_of_bc = function
  | Move -> "move"
  | RotateLeft -> "RotateLeft"
  | RotateRight -> "RotateRight"
  | Color c -> "Color " ^ string_of_color c

type square = {
  star: bool;
  color: color
}
	       
type puzzle = {
  t: square option array array;
  w: int;
  h: int
}

type state = {
  position: G.point;
  orientation: G.vector;
  board: puzzle;
  stars: int
}

let print_puzzle {t; w; h} =
  let open Printf in
  let open String in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      match t.(i).(j) with
      | None -> printf "."
      | Some {star = s; color = c} ->
	 let cs = string_of_color c in
	 printf "%s" (if (s: bool) then uppercase cs else cs)
    done;
    print_newline ()
  done

let is_in {w; h} (G.P (x,y)) =
  x >= 0 && x < w && y >= 0 && y < h

let next p o = G.move p o

let move p o b =
  let p' = next p o in
  if p' |> is_in b then
    p'
  else
    raise Game_over
    
let color p ({t = b} as puzzle) c =
  let open Geometry in
  let (P (x,y)) = p in
  match b.(y).(x) with
  | None -> assert false
  | Some ({color = _} as square) ->
     let square' = {square with color = c} in
     b.(y).(x) <- Some square';
     {puzzle with t = b}
	       
let step ({position = p;			 
	   orientation = o;
	   board = b;
	   stars = s} as state) = function
  | Move ->
     let p' = move p o b in
     {state with position = p'}
  | RotateLeft ->
     let o' = G.rotate_left o in
     {state with orientation = o'}
  | RotateRight ->
     let o' = G.rotate_right o in
     {state with orientation = o'}
  | Color c ->
     let b' = color p b c in
     {state with board = b'}
     
