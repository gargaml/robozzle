open Sdlvideo

open Vm

module G = Geometry

type tile =
    { position: Sdlvideo.rect;
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

let create_tile x y w h =
  let open Int32 in
  let m0 = of_int 0xff in
  let m1 = shift_left m0 8 in
  let m2 = shift_left m1 8 in
  let m3 = shift_left m2 8 in
  let s = create_RGB_surface [`HWSURFACE] w h 32 m0 m1 m2 m3 in
  { position = rect x y w h; surface = s }

let tile = create_tile 0 0 0 0

let create_grid l c w h =
  let ts = Array.make l (Array.make 0 tile) in
  Array.iteri (fun i _ -> ts.(i) <- Array.make c tile) ts;
  for i = 0 to l - 1 do
    for j = 0 to c - 1 do
      ts.(i).(j) <- create_tile (j * w) (i * h) w h
    done
  done;
  { lines = l; columns = c; tiles = ts }

let init_graphics w h lines columns  =
  Sdl.init [`VIDEO;`TIMER];
  let s = Sdlvideo.set_video_mode w h [`DOUBLEBUF;`HWSURFACE] in
  let tw = w / (columns + 2) and th = h / (lines + 2) in
  let g = create_grid lines columns tw th in
  { width = w;
    height = h;
    screen = s;
    grid = g }

let draw_tile {position; surface} {star; color} =
  let c = match color with
    | Black -> map_RGB surface black
    | Red -> map_RGB surface red
    | Green -> map_RGB surface green
    | Blue -> map_RGB surface blue
  in
  fill_rect surface c;
  if star then
    let {r_x; r_y; r_w; r_h} = position in
    let r = rect (r_w / 4) (r_h / 4) (r_w / 2) (r_h / 2) in
    fill_rect ~rect:r surface (map_RGB surface yellow)

let draw_grid {screen; grid = {lines;columns;tiles}} state =
  let open Printf in
  let {position = p; orientation = o; board = b} = state in
  for i = 0 to lines - 1 do
    for j = 0 to columns - 1 do
      let tile = tiles.(i).(j) in
      draw_tile tile b.table.(i).(j);
      blit_surface ~src:tile.surface ~dst:screen ~dst_rect:tile.position ()
    done
  done

let draw_bot {screen; grid = {tiles}} {position;orientation} =
  let x,y = G.get_coordinates position in
  let ({position;surface} as tile) = tiles.(y).(x) in
  fill_rect surface (map_RGB surface magenta);
  blit_surface ~src:surface ~dst:screen ~dst_rect:position ()
    
let refresh info state =
  draw_grid info state;
  draw_bot info state;
  flip info.screen
