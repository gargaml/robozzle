open Vm
open Yojson.Basic

module G = Geometry

let deserialize_orientation =
  let os = [(0,1);(1,0);(0,-1);(-1,0)] |> List.map (fun (u,v) -> G.make_vector u v)
  in fun n -> List.nth os n
       
let deserialize json =
  let open Util in
  let width = member "width" json |> to_int in
  let height = member "height" json |> to_int in
  let board = member "board" json |> to_string in
  let x = member "robotCol" json |> to_int in
  let y = member "robotRow" json |> to_int in
  let o = deserialize_orientation (member "robotDir" json |> to_int) in
  let t = Array.make_matrix height width {star = false; color = Black} in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      t.(i).(j) <-
	match String.get board ((i * width) + j) with
	| '.' -> {star = false; color = Black}
	| 'r' -> {star = false; color = Red}
	| 'R' -> {star = true; color = Red}
	| 'g' -> {star = false; color = Green}
	| 'G' -> {star = true; color = Green}
	| 'b' -> {star = false; color = Blue}
	| 'B' -> {star = true; color = Blue}
	| _ -> failwith "Character not expected."
    done
  done;
  { table = t;
    lines = height;
    columns = width;
    start_position = G.make_point x y;
    start_orientation = o }
	   
let puzzle filename = from_file filename |> deserialize
