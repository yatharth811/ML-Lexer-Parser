structure AST = struct
	type ID = string
    datatype TYP = INT | BOOL
    datatype IDSEQ = IDS of ID | IDSEQ of ID * IDSEQ
    datatype DEC = DEC of IDSEQ * TYP
    datatype DECSEQ = DECS | DECSEQ of DEC * DECSEQ
    datatype EXP = NUM of int
        | SET of ID * EXP
		| ITE of EXP * CMDSEQ * CMDSEQ
		| WH of EXP * CMDSEQ
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
        | ID     
    and CMDSEQ = CMDS | CMDSEQ of EXP * CMDSEQ
    datatype BLK = BLK of DECSEQ * CMDSEQ
    datatype PROG = PROG of ID * BLK
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

*)