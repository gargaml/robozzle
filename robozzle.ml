open Vm

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
	     
let init () =
  Sdl.init [`VIDEO;`TIMER];
  at_exit Sdl.quit;
  Sdlvideo.set_video_mode w h [`DOUBLEBUF;`HWSURFACE]
  
let _ =
  let screen = init () in
  let p = Load.puzzle (Sys.argv.(1)) in
  print_puzzle p;
  loop ()
