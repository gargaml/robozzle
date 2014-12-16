{
  open Lexing
  open Parser
}

let whitespace = [' ' '\t']+
let newline = '\n'+
let colon = ':'

let move = "move"
let rotate = "rotate"
let color = "color"	       
let call = "call"
let name = ['a'-'z' 'A'-'Z']

rule read =
  parse
  | whitespace { read lexbuf }
  | newline { read lexbuf }
  | colon { COLON }
  | "red" { RED }
  | "green" { GREEN }
  | "blue" { BLUE }
  | "left" { LEFT }
  | "right" { RIGHT }
  | name { NAME (Lexing.lexeme lexbuf) }
  | move { MOVE }
  | rotate { ROTATE }
  | color { COLOR }
  | call { CALL }
	 
