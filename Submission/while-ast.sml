structure AST = struct
    datatype TYP = INT | BOOL
    datatype VAR = VAR of string
    type IDSEQ = VAR list
    datatype DEC = DEC of IDSEQ * TYP
    type DECWRAP = DEC list
    datatype DECSEQ = DECSEQ of DECWRAP
    datatype CMD = SET of string * EXP
        |  ITE of EXP * CMDSEQ * CMDSEQ
        | WH of EXP * CMDSEQ
        | READ of string
        | WRITE of EXP
    and EXP = NUM of int
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
        | VAREXP of string 
    and CMDSEQ = CMDSEQ of CMD list
    type CMDWRAP = CMD list
    datatype BLK = BLK of DECSEQ * CMDSEQ
    datatype PROG = PROG of string * BLK
    fun decAdd(dec : DEC, seq : DECWRAP ) = dec :: seq
    fun idAdd(hm: VAR, seq: IDSEQ ) = hm :: seq
    fun cmdAdd(ex : CMD, seq: CMDWRAP) = ex :: seq
    
    (* datatype TYPEVAL = INTTYPE | BOOLTYPE
    type TYPEENV = (string * TYPEVAL) list

    type environment = (id * value) list
    fun envAdd (var:id, v:value, env:environment) =
    (var,v)::env

    fun envAddAll ([] , env2:environment) = env2
    | envAddAll (x::env : environment , env2:environment) = envAddAll(env, x::env2)  

    fun envLookup (var:id, env:environment) =
        case List.find(fn (x, _) => x = var) env of
                            SOME (x, v)   => v
                        |   NONE => raise Fail ("Error: unbound variable: "^var)
                                
    fun envRLookup (var:value, env:environment) =
        case List.find(fn (_, x) => x = var) env of
                            SOME (v, x)   => v
                        |   NONE => ""

    fun envDelete (var:id, []) = []
    | envDelete (var:id, y::ys) = let val (s,_) = y in if s = var then envDelete(var,ys) else y::envDelete(var,ys) end *)
end