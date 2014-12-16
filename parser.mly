%token COLON

%token RED GREEN BLUE
%token LEFT RIGHT

%token <string> NAME
%token MOVE
%token ROTATE
%token COLOR
%token CALL

%{
  open Ast
%}

%start <Ast.program> program
%%

color:
| RED { Red }
| GREEN { Green }
| BLUE { Blue }
;

direction:
| RIGHT { Right }
| LEFT { Left }
;
  
inst:
| MOVE { Move }
| ROTATE d = direction { Rotate d }
| COLOR c = color { Color c }
| CALL n = NAME { Call n }
;
  
procedure:
| l = NAME COLON is = list(inst) { Procedure (l,is) }
;
  
program:
| ps = list(procedure) { Program ps }
;
