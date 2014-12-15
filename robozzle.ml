open Parser
open Vm
open Graphics

open Sdl

module G = Geometry

let w = 800
let h = 600

let loop () =
  let open Sdlevent in
  let keep_on = ref true in
  while !keep_on do
    match wait_event () with
    | KEYUP kb -> keep_on := false
    | _ -> ()
  done
	     
let init filename =
  let p = Load.puzzle filename in
  let g = init_graphics w h p.lines p.columns in
  let s = { position = p.start_position;
	    orientation = p.start_orientation;
	    board = p;
	    stars = 0 } in
  g, s

let eval info state bs =
  let rec loop state = function
    | [] -> ()
    | b :: bs ->
       let state' = step state b in
       refresh info state';
       loop state' bs
  in loop state bs

let bs = [Move]
	  
let _ =
  let ic = open_in Sys.argv.(1) in
  let p = Parser.program Lexer.read (Lexing.from_channel ic) in
  let bcs = Compiler.compile p in
  let info, state = init Sys.argv.(1) in
  eval info state bs;
  loop ();
  Sdl.quit ()
