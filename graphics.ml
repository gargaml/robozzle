open Sdlvideo

open Vm

type tile =
    { width: int;
      height: int;
      surface: Sdlvideo.surface }

type grid =
    { lines: int;
      columns: int;
      tiles: tile array array }
       
type info =
    { width: int;
      height: int;
      screen: Sdlvideo.surface;
      grid: grid }

let create_tile w h =
  let open Int32 in
  let m0 = of_int 0xff in
  let m1 = shift_left m0 8 in
  let m2 = shift_left m1 8 in
  let m3 = shift_left m2 8 in
  let s = create_RGB_surface [`HWSURFACE] w h 32 m0 m1 m2 m3 in
  { width = w; height = h; surface = s }

let tile = create_tile 0 0

let create_grid l c w h =
  let ts = Array.make_matrix l c tile in
  for i = 0 to l - 1 do
    for j = 0 to c - 1 do
      ts.(i).(j) <- create_tile w h
    done
  done;
  { lines = l; columns = c; tiles = ts }

let init_graphics w h x y  =
  Sdl.init [`VIDEO;`TIMER];
  let s = Sdlvideo.set_video_mode w h [`DOUBLEBUF;`HWSURFACE] in
  let tw = w / (x + 2) and th = h / (y + 2) in
  let g = create_grid x y tw th in
  { width = w;
    height = h;
    screen = s;
    grid = g }

let draw_tile {surface} {star; color} =
  let c = match color with
    | Red -> map_RGB surface red
    | Green -> map_RGB surface green
    | Blue -> map_RGB surface blue
  in
  fill_rect surface c

let draw_grid {screen; grid = {lines;columns;tiles}} state =
  let {position = p; orientation = o; board = b} = state in
  for i = 0 to lines - 1 do
    for j = 0 to columns - 1 do
      ()
    done
  done;
  flip screen
       
let refresh info state =
  ()
