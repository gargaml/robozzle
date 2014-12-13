open Vm
open Graphics

open Sdl

module G = Geometry

let w = 800
let h = 600

type info =
    { width: int;
      height: int;
      screen: Sdlvideo.surface;
      state: Vm.state }

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
  let g = init_graphics w h p.w p.w in
  let s = {position = G.make_point 0 0;
	   orientation = G.make_vector 0 1;
	   board = p;
	   stars = 0} in
  g, s
  
let _ =
  let info, state = init Sys.argv.(1) in
  draw_grid info state;
  print_puzzle state.board;
  loop ()
