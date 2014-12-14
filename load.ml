open Vm
open Yojson.Basic

let deserialize json =
  let open Util in
  let width = member "width" json |> to_int in
  let height = member "height" json |> to_int in
  let board = member "board" json |> to_string in
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
  {table = t; lines = height; columns = width}
	   
let puzzle filename = from_file filename |> deserialize
