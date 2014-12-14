type point = P of int * int

type vector = V of int * int

let make_point x y = P(x,y)

let get_x (P(x,y)) = x

let get_y (P(x,y)) = y

let make_vector u v = V (u,v)

let get_coordinates (P(x,y)) = (x,y)

let make_vector u v = V(u,v)

let move (P (x,y)) (V (u,v)) = P (x+u, y+v)

let rotate_left (V (u,v)) = V (-v,u)

let rotate_right (V (u,v)) = V (v,-u)
			       
