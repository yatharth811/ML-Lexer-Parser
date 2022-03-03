structure AST = struct
	(* type ID = string *)
    datatype TYP = INT | BOOL
    type IDSEQ = string list
    datatype DEC = DEC of IDSEQ * TYP
    type DECSEQ = DEC list
    datatype EXP = NUM of int
        | SET of string * EXP
		| ITE of EXP * EXP list * EXP list
		| WH of EXP * EXP list
        | PLUS of EXP * EXP
        | MINUS of EXP * EXP
        | TIMES of EXP * EXP
        | DIV of EXP * EXP
        | MOD of EXP * EXP
        | LT of EXP * EXP
        | LEQ of EXP * EXP
        | EQ of EXP * EXP
        | GT of EXP * EXP
        | GEQ of EXP * EXP
		| NEQ of EXP * EXP
		| NEGATE of EXP
        | OR of EXP * EXP
        | AND of EXP * EXP
		| NOT of EXP
        | TT
        | FF
        | VARIABLE of string 
    type CMDSEQ = EXP list
    datatype BLK = BLK of DECSEQ * CMDSEQ
    datatype PROG = PROG of string * BLK
    fun decAdd(dec : DEC, seq : DECSEQ ) = dec :: seq
    fun idAdd(hm: string, seq: IDSEQ ) = hm :: seq
    fun cmdAdd(ex : EXP, seq: CMDSEQ) = ex :: seq 
end

	(* datatype value = IntVal of int | BoolVal of bool | FunVal of id * typ * typ * EXP *)
	
        (* | Fn of id * typ * typ * EXP
		| VarExp of id
		| BinExp of binop * EXP * EXP *)
		(* | AppExp of id * EXP *)
		(* | LetExp of id * EXP * EXP *)
		(* | Fun of id * id * typ * typ * EXP *)
	
	(* datatype lines = statement of EXP | statements of EXP * lines *)
	
	(* type environment = (id * value) list *)
(* 	
	fun envAdd (var:id, v:value, env:environment) =
		case List.find(fn (x, _) => x = var) env of
			SOME(x, v) => raise Fail ("Variable " ^ var ^ " already declared.")
			| NONE => (var,v)::env *)
	
	(* fun envUpdate (var:id, v:value , env : environment) =
		case List.find(fn (x, _) => x = var) env of
			SOME(x, v)	=> ((var,v)::(List.filter(fn y => not(y = (x, v))) env))
			| NONE		=> (var,v)::env *)
		
	
	(* fun envLookup (var:id, env:environment) =
		case List.find(fn (x, _) => x = var) env of
			SOME(x, v) => v
			| NONE => raise Fail ("Variable " ^ var ^ " not declared yet.") *)

(*
%nonterm START of AST.exp | BLK of AST.blk | DEC of AST.dec| CMDSEQ of AST.seq | TYPE of AST.typ | VARLIST of AST.seq | EXP of AST.exp | CMDWRAP of AST.seq | DECSEQ of AST.seq

%left LT LEQ EQ GT GEQ NEQ
%left MOD PLUS MINUS
%left TIMES DIV
%right IF THEN ELSE WHILE DO
%nonassoc ENDIF ENDWH
%left OR AND

%right NOT 
%right NEGATE
%right SET

*)