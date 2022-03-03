functor whileLexFun (structure Tokens : while_TOKENS)=
   struct
    structure UserDeclarations =
      struct
structure  Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token
exception badCharacter
val pos = ref 1
val col = ref 1
val eof = fn () => (Tokens.EOF(!pos, !pos))

end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\076\077\003\003\076\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\076\075\003\003\003\074\072\003\071\070\069\068\067\066\003\065\
\\003\064\064\064\064\064\064\064\064\064\061\060\057\056\054\003\
\\003\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\003\003\003\003\003\
\\003\010\050\010\048\038\036\010\010\032\010\010\010\010\010\010\
\\025\010\010\010\020\010\017\012\010\010\010\009\007\006\004\003\
\\003"
),
 (4, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (5, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (7, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\008\000\000\000\
\\000"
),
 (10, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\013\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (13, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\014\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\015\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (15, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\016\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (17, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\018\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\019\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (20, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\022\011\011\011\011\011\011\011\
\\011\011\011\011\021\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (22, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\023\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (23, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\024\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (25, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\026\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (26, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\027\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (27, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\028\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (28, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\029\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (29, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\030\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (30, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\031\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (32, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\035\011\011\011\011\011\011\011\033\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (33, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\034\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (36, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\037\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (38, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\045\011\039\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (39, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\040\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (40, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\043\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\041\011\011\011\000\000\000\000\000\
\\000"
),
 (41, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\042\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (43, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\044\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (45, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\046\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (46, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\047\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (48, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\049\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (50, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\051\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (51, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\052\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (52, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\011\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000\011\011\011\011\011\011\011\011\011\011\011\053\011\011\011\
\\011\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\
\\000"
),
 (54, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\055\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (57, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\059\058\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (61, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\063\000\000\062\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (72, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\073\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (76, 
"\000\000\000\000\000\000\000\000\000\077\077\000\000\077\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\077\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 127)], trans = 0},
{fin = [(N 5),(N 127)], trans = 4},
{fin = [(N 3)], trans = 5},
{fin = [(N 21),(N 127)], trans = 0},
{fin = [(N 127)], trans = 7},
{fin = [(N 104)], trans = 0},
{fin = [(N 19),(N 127)], trans = 0},
{fin = [(N 122),(N 127)], trans = 10},
{fin = [(N 122)], trans = 10},
{fin = [(N 122),(N 127)], trans = 12},
{fin = [(N 122)], trans = 13},
{fin = [(N 122)], trans = 14},
{fin = [(N 122)], trans = 15},
{fin = [(N 81),(N 122)], trans = 10},
{fin = [(N 122),(N 127)], trans = 17},
{fin = [(N 122)], trans = 18},
{fin = [(N 47),(N 122)], trans = 10},
{fin = [(N 122),(N 127)], trans = 20},
{fin = [(N 93),(N 122)], trans = 10},
{fin = [(N 122)], trans = 22},
{fin = [(N 122)], trans = 23},
{fin = [(N 64),(N 122)], trans = 10},
{fin = [(N 122),(N 127)], trans = 25},
{fin = [(N 122)], trans = 26},
{fin = [(N 122)], trans = 27},
{fin = [(N 122)], trans = 28},
{fin = [(N 122)], trans = 29},
{fin = [(N 122)], trans = 30},
{fin = [(N 43),(N 122)], trans = 10},
{fin = [(N 122),(N 127)], trans = 32},
{fin = [(N 122)], trans = 33},
{fin = [(N 51),(N 122)], trans = 10},
{fin = [(N 59),(N 122)], trans = 10},
{fin = [(N 122),(N 127)], trans = 36},
{fin = [(N 96),(N 122)], trans = 10},
{fin = [(N 122),(N 127)], trans = 38},
{fin = [(N 122)], trans = 39},
{fin = [(N 122)], trans = 40},
{fin = [(N 122)], trans = 41},
{fin = [(N 90),(N 122)], trans = 10},
{fin = [(N 122)], trans = 43},
{fin = [(N 75),(N 122)], trans = 10},
{fin = [(N 122)], trans = 45},
{fin = [(N 122)], trans = 46},
{fin = [(N 69),(N 122)], trans = 10},
{fin = [(N 122),(N 127)], trans = 48},
{fin = [(N 84),(N 122)], trans = 10},
{fin = [(N 122),(N 127)], trans = 50},
{fin = [(N 122)], trans = 51},
{fin = [(N 122)], trans = 52},
{fin = [(N 56),(N 122)], trans = 10},
{fin = [(N 116),(N 127)], trans = 54},
{fin = [(N 119)], trans = 0},
{fin = [(N 114),(N 127)], trans = 0},
{fin = [(N 109),(N 127)], trans = 57},
{fin = [(N 107)], trans = 0},
{fin = [(N 112)], trans = 0},
{fin = [(N 7),(N 127)], trans = 0},
{fin = [(N 12),(N 127)], trans = 61},
{fin = [(N 15)], trans = 0},
{fin = [(N 10)], trans = 0},
{fin = [(N 3),(N 127)], trans = 5},
{fin = [(N 33),(N 127)], trans = 0},
{fin = [(N 29),(N 127)], trans = 0},
{fin = [(N 17),(N 127)], trans = 0},
{fin = [(N 27),(N 127)], trans = 4},
{fin = [(N 31),(N 127)], trans = 0},
{fin = [(N 25),(N 127)], trans = 0},
{fin = [(N 23),(N 127)], trans = 0},
{fin = [(N 127)], trans = 72},
{fin = [(N 101)], trans = 0},
{fin = [(N 35),(N 127)], trans = 0},
{fin = [(N 98),(N 127)], trans = 0},
{fin = [(N 125),(N 127)], trans = 76},
{fin = [(N 125)], trans = 76}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  10 => let val yytext=yymktext() in col := !col + 2; print("DCOLON " ^ yytext ^ "\n"); Tokens.DCOLON(!pos, !pos) end
| 101 => let val yytext=yymktext() in col := !col + 2; print("AND " ^ yytext ^ "\n");  Tokens.AND(!pos, !pos) end
| 104 => let val yytext=yymktext() in col := !col + 2; print("OR " ^ yytext ^ "\n");  Tokens.OR(!pos, !pos) end
| 107 => let val yytext=yymktext() in col := !col + 2; print("NEQ " ^ yytext ^ "\n");  Tokens.NEQ(!pos, !pos) end
| 109 => let val yytext=yymktext() in col := !col + 1; print("LT " ^ yytext ^ "\n");  Tokens.LT(!pos, !pos) end
| 112 => let val yytext=yymktext() in col := !col + 2; print("LEQ " ^ yytext ^ "\n");  Tokens.LEQ(!pos, !pos) end
| 114 => let val yytext=yymktext() in col := !col + 1; print("EQ " ^ yytext ^ "\n");  Tokens.EQ(!pos, !pos) end
| 116 => let val yytext=yymktext() in col := !col + 1; print("GT " ^ yytext ^ "\n");  Tokens.GT(!pos, !pos) end
| 119 => let val yytext=yymktext() in col := !col + 2; print("GEQ " ^ yytext ^ "\n");  Tokens.GEQ(!pos, !pos) end
| 12 => let val yytext=yymktext() in col := !col + 1; print("COLON " ^ yytext ^ "\n"); Tokens.COLON(!pos, !pos) end
| 122 => let val yytext=yymktext() in col := !col + size(yytext); print("ID " ^ yytext ^ "\n");  Tokens.ID(yytext, !pos, !pos) end
| 125 => let val yytext=yymktext() in col := !col + size yytext; lex() end
| 127 => (raise badCharacter; lex())
| 15 => let val yytext=yymktext() in col := !col + 2; print("SET " ^ yytext ^ "\n"); Tokens.SET(!pos, !pos) end
| 17 => let val yytext=yymktext() in col := !col + 1; print("COMMA " ^ yytext ^ "\n");  Tokens.COMMA(!pos, !pos) end
| 19 => let val yytext=yymktext() in col := !col + 1; print("LBRACE " ^ yytext ^ "\n");  Tokens.LBRACE(!pos, !pos) end
| 21 => let val yytext=yymktext() in col := !col + 1; print("RBRACE " ^ yytext ^ "\n");  Tokens.RBRACE(!pos, !pos) end
| 23 => let val yytext=yymktext() in col := !col + 1; print("LPAREN " ^ yytext ^ "\n");  Tokens.LPAREN(!pos, !pos) end
| 25 => let val yytext=yymktext() in col := !col + 1; print("RPAREN " ^ yytext ^ "\n");  Tokens.RPAREN(!pos, !pos) end
| 27 => let val yytext=yymktext() in col := !col + 1; print("PLUS " ^ yytext ^ "\n");  Tokens.PLUS(!pos, !pos) end
| 29 => let val yytext=yymktext() in col := !col + 1; print("MINUS " ^ yytext ^ "\n");  Tokens.MINUS(!pos, !pos) end
| 3 => let val yytext=yymktext() in col := !col + size(yytext); print("NUM " ^ yytext ^ "\n"); Tokens.NUM(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext), !pos, !pos) end
| 31 => let val yytext=yymktext() in col := !col + 1; print("TIMES " ^ yytext ^ "\n");  Tokens.TIMES(!pos, !pos) end
| 33 => let val yytext=yymktext() in col := !col + 1; print("DIV " ^ yytext ^ "\n");  Tokens.DIV(!pos, !pos) end
| 35 => let val yytext=yymktext() in col := !col + 1; print("MOD " ^ yytext ^ "\n");  Tokens.MOD(!pos, !pos) end
| 43 => let val yytext=yymktext() in col := !col + 7; print("PROG " ^ yytext ^ "\n");  Tokens.PROG(!pos, !pos) end
| 47 => let val yytext=yymktext() in col := !col + 3; print("VAR " ^ yytext ^ "\n");  Tokens.VAR(!pos, !pos) end
| 5 => let val yytext=yymktext() in col := !col + 1; print("NEGATE " ^ yytext ^ "\n");  Tokens.NEGATE(!pos, !pos) end
| 51 => let val yytext=yymktext() in col := !col + 3; print("INT " ^ yytext ^ "\n");  Tokens.INT(!pos, !pos) end
| 56 => let val yytext=yymktext() in col := !col + 4; print("BOOL " ^ yytext ^ "\n");  Tokens.BOOL(!pos, !pos) end
| 59 => let val yytext=yymktext() in col := !col + 2; print("ITE " ^ yytext ^ "\n");  Tokens.IF(!pos, !pos) end
| 64 => let val yytext=yymktext() in col := !col + 4; print("ITE " ^ yytext ^ "\n");  Tokens.THEN(!pos, !pos) end
| 69 => let val yytext=yymktext() in col := !col + 4; print("ITE " ^ yytext ^ "\n");  Tokens.ELSE(!pos, !pos) end
| 7 => let val yytext=yymktext() in col := !col + 1; print("TERM " ^ yytext ^ "\n");  Tokens.TERM(!pos, !pos) end
| 75 => let val yytext=yymktext() in col := !col + 5; print("ITE " ^ yytext ^ "\n");  Tokens.ENDIF(!pos, !pos) end
| 81 => let val yytext=yymktext() in col := !col + 5; print("WH " ^ yytext ^ "\n");  Tokens.WHILE(!pos, !pos) end
| 84 => let val yytext=yymktext() in col := !col + 2; print("WH " ^ yytext ^ "\n");  Tokens.DO(!pos, !pos) end
| 90 => let val yytext=yymktext() in col := !col + 5; print("WH " ^ yytext ^ "\n");  Tokens.ENDWH(!pos, !pos) end
| 93 => let val yytext=yymktext() in col := !col + 2; print("TT " ^ yytext ^ "\n");  Tokens.TT(!pos, !pos) end
| 96 => let val yytext=yymktext() in col := !col + 2; print("FF " ^ yytext ^ "\n");  Tokens.FF(!pos, !pos) end
| 98 => let val yytext=yymktext() in col := !col + 1; print("NOT " ^ yytext ^ "\n");  Tokens.NOT(!pos, !pos) end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end