open Vm
open Yojson.Basic

let deserialize json =
  let open Util in
  let width = member "width" json |> to_int in
  let height = member "height" json |> to_int in
  let board = member "board" json |> to_string in
  let t = Array.make_matrix height width None in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      t.(i).(j) <-
	match String.get board ((i * width) + j) with
	| '.' -> None
	| 'r' -> Some {star = false; color = Red}
	| 'R' -> Some {star = true; color = Red}
	| 'g' -> Some {star = false; color = Green}
	| 'G' -> Some {star = true; color = Green}
	| 'b' -> Some {star = false; color = Blue}
	| 'B' -> Some {star = true; color = Blue}
	| _ -> failwith "Character not expected."
    done
  done;
  {t = t; w = width; h = height}
	   
let puzzle filename = from_file filename |> deserialize
