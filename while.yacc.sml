functor whileLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : while_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct




end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\026\000\009\000\025\000\020\000\024\000\024\000\023\000\
\\027\000\022\000\028\000\021\000\029\000\020\000\038\000\019\000\
\\039\000\018\000\000\000\
\\001\000\002\000\043\000\011\000\042\000\012\000\041\000\013\000\040\000\
\\014\000\039\000\015\000\038\000\030\000\037\000\031\000\036\000\
\\032\000\035\000\033\000\034\000\034\000\033\000\035\000\032\000\
\\036\000\031\000\037\000\030\000\000\000\
\\001\000\002\000\072\000\000\000\
\\001\000\003\000\005\000\000\000\
\\001\000\004\000\027\000\000\000\
\\001\000\007\000\012\000\000\000\
\\001\000\008\000\029\000\000\000\
\\001\000\010\000\071\000\011\000\042\000\012\000\041\000\013\000\040\000\
\\014\000\039\000\015\000\038\000\030\000\037\000\031\000\036\000\
\\032\000\035\000\033\000\034\000\034\000\033\000\035\000\032\000\
\\036\000\031\000\037\000\030\000\000\000\
\\001\000\011\000\042\000\012\000\041\000\013\000\040\000\014\000\039\000\
\\015\000\038\000\021\000\070\000\030\000\037\000\031\000\036\000\
\\032\000\035\000\033\000\034\000\034\000\033\000\035\000\032\000\
\\036\000\031\000\037\000\030\000\000\000\
\\001\000\011\000\042\000\012\000\041\000\013\000\040\000\014\000\039\000\
\\015\000\038\000\025\000\069\000\030\000\037\000\031\000\036\000\
\\032\000\035\000\033\000\034\000\034\000\033\000\035\000\032\000\
\\036\000\031\000\037\000\030\000\000\000\
\\001\000\016\000\003\000\000\000\
\\001\000\018\000\052\000\019\000\051\000\000\000\
\\001\000\022\000\078\000\000\000\
\\001\000\023\000\076\000\000\000\
\\001\000\026\000\075\000\000\000\
\\001\000\038\000\004\000\000\000\
\\001\000\038\000\015\000\000\000\
\\001\000\040\000\000\000\000\000\
\\080\000\000\000\
\\081\000\000\000\
\\082\000\000\000\
\\083\000\017\000\010\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\006\000\028\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\001\000\026\000\009\000\025\000\020\000\024\000\024\000\023\000\
\\027\000\022\000\028\000\021\000\029\000\020\000\038\000\019\000\
\\039\000\018\000\000\000\
\\092\000\000\000\
\\093\000\011\000\042\000\012\000\041\000\013\000\040\000\014\000\039\000\
\\015\000\038\000\030\000\037\000\031\000\036\000\032\000\035\000\
\\033\000\034\000\034\000\033\000\035\000\032\000\036\000\031\000\
\\037\000\030\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\013\000\040\000\014\000\039\000\015\000\038\000\000\000\
\\097\000\013\000\040\000\014\000\039\000\015\000\038\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\011\000\042\000\012\000\041\000\013\000\040\000\014\000\039\000\
\\015\000\038\000\000\000\
\\102\000\011\000\042\000\012\000\041\000\013\000\040\000\014\000\039\000\
\\015\000\038\000\000\000\
\\103\000\011\000\042\000\012\000\041\000\013\000\040\000\014\000\039\000\
\\015\000\038\000\033\000\034\000\034\000\033\000\036\000\031\000\
\\037\000\030\000\000\000\
\\104\000\011\000\042\000\012\000\041\000\013\000\040\000\014\000\039\000\
\\015\000\038\000\000\000\
\\105\000\011\000\042\000\012\000\041\000\013\000\040\000\014\000\039\000\
\\015\000\038\000\000\000\
\\106\000\011\000\042\000\012\000\041\000\013\000\040\000\014\000\039\000\
\\015\000\038\000\033\000\034\000\034\000\033\000\036\000\031\000\
\\037\000\030\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\011\000\042\000\012\000\041\000\013\000\040\000\014\000\039\000\
\\015\000\038\000\030\000\037\000\032\000\035\000\033\000\034\000\
\\034\000\033\000\035\000\032\000\036\000\031\000\037\000\030\000\000\000\
\\111\000\011\000\042\000\012\000\041\000\013\000\040\000\014\000\039\000\
\\015\000\038\000\032\000\035\000\033\000\034\000\034\000\033\000\
\\035\000\032\000\036\000\031\000\037\000\030\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\005\000\044\000\000\000\
\\115\000\000\000\
\"
val actionRowNumbers =
"\010\000\015\000\003\000\021\000\
\\005\000\020\000\021\000\018\000\
\\016\000\019\000\029\000\022\000\
\\004\000\024\000\006\000\001\000\
\\045\000\052\000\000\000\051\000\
\\050\000\000\000\000\000\000\000\
\\000\000\011\000\016\000\028\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\029\000\000\000\053\000\
\\009\000\008\000\007\000\047\000\
\\002\000\027\000\026\000\025\000\
\\043\000\042\000\041\000\040\000\
\\039\000\044\000\048\000\049\000\
\\038\000\037\000\036\000\035\000\
\\034\000\030\000\031\000\005\000\
\\005\000\046\000\023\000\014\000\
\\013\000\033\000\005\000\012\000\
\\032\000\017\000"
val gotoT =
"\
\\001\000\077\000\000\000\
\\000\000\
\\000\000\
\\002\000\007\000\003\000\006\000\009\000\005\000\010\000\004\000\000\000\
\\004\000\009\000\000\000\
\\000\000\
\\003\000\006\000\009\000\011\000\000\000\
\\000\000\
\\006\000\012\000\000\000\
\\000\000\
\\007\000\015\000\008\000\014\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\043\000\000\000\
\\000\000\
\\000\000\
\\007\000\044\000\000\000\
\\007\000\045\000\000\000\
\\007\000\046\000\000\000\
\\007\000\047\000\000\000\
\\005\000\048\000\000\000\
\\006\000\051\000\000\000\
\\000\000\
\\007\000\052\000\000\000\
\\007\000\053\000\000\000\
\\007\000\054\000\000\000\
\\007\000\055\000\000\000\
\\007\000\056\000\000\000\
\\007\000\057\000\000\000\
\\007\000\058\000\000\000\
\\007\000\059\000\000\000\
\\007\000\060\000\000\000\
\\007\000\061\000\000\000\
\\007\000\062\000\000\000\
\\007\000\063\000\000\000\
\\007\000\064\000\000\000\
\\007\000\015\000\008\000\065\000\000\000\
\\007\000\066\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\071\000\000\000\
\\004\000\072\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\075\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 78
val numrules = 36
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUM of unit ->  (int) | ID of unit ->  (string)
 | DECSEQ of unit ->  (AST.DECSEQ) | DECWRAP of unit ->  (AST.DECWRAP)
 | CMDWRAP of unit ->  (AST.CMDWRAP) | EXP of unit ->  (AST.EXP)
 | VARLIST of unit ->  (AST.IDSEQ) | TYPE of unit ->  (AST.TYP)
 | CMDSEQ of unit ->  (AST.CMDSEQ) | DEC of unit ->  (AST.DEC)
 | BLK of unit ->  (AST.BLK) | START of unit ->  (AST.PROG)
end
type svalue = MlyValue.svalue
type result = AST.PROG
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 39) => true | _ => false
val showTerminal =
fn (T 0) => "NEGATE"
  | (T 1) => "TERM"
  | (T 2) => "DCOLON"
  | (T 3) => "COLON"
  | (T 4) => "SET"
  | (T 5) => "COMMA"
  | (T 6) => "LBRACE"
  | (T 7) => "RBRACE"
  | (T 8) => "LPAREN"
  | (T 9) => "RPAREN"
  | (T 10) => "PLUS"
  | (T 11) => "MINUS"
  | (T 12) => "TIMES"
  | (T 13) => "DIV"
  | (T 14) => "MOD"
  | (T 15) => "PROG"
  | (T 16) => "VAR"
  | (T 17) => "INT"
  | (T 18) => "BOOL"
  | (T 19) => "IF"
  | (T 20) => "THEN"
  | (T 21) => "ENDIF"
  | (T 22) => "ELSE"
  | (T 23) => "WHILE"
  | (T 24) => "DO"
  | (T 25) => "ENDWH"
  | (T 26) => "TT"
  | (T 27) => "FF"
  | (T 28) => "NOT"
  | (T 29) => "AND"
  | (T 30) => "OR"
  | (T 31) => "NEQ"
  | (T 32) => "LT"
  | (T 33) => "LEQ"
  | (T 34) => "EQ"
  | (T 35) => "GT"
  | (T 36) => "GEQ"
  | (T 37) => "ID"
  | (T 38) => "NUM"
  | (T 39) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 39) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 
2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.BLK BLK1, _, BLK1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, PROG1left, _)) :: rest671)) =>
 let val  result = MlyValue.START (fn _ => let val  (ID as ID1) = ID1
 ()
 val  (BLK as BLK1) = BLK1 ()
 in (AST.PROG(ID, BLK))
end)
 in ( LrTable.NT 0, ( result, PROG1left, BLK1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.CMDSEQ CMDSEQ1, _, CMDSEQ1right)) :: ( _, ( 
MlyValue.DECSEQ DECSEQ1, DECSEQ1left, _)) :: rest671)) => let val  
result = MlyValue.BLK (fn _ => let val  (DECSEQ as DECSEQ1) = DECSEQ1
 ()
 val  (CMDSEQ as CMDSEQ1) = CMDSEQ1 ()
 in (AST.BLK(DECSEQ, CMDSEQ))
end)
 in ( LrTable.NT 1, ( result, DECSEQ1left, CMDSEQ1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.DECWRAP DECWRAP1, DECWRAP1left, 
DECWRAP1right)) :: rest671)) => let val  result = MlyValue.DECSEQ (fn
 _ => let val  (DECWRAP as DECWRAP1) = DECWRAP1 ()
 in (AST.DECSEQ(DECWRAP))
end)
 in ( LrTable.NT 9, ( result, DECWRAP1left, DECWRAP1right), rest671)

end
|  ( 3, ( rest671)) => let val  result = MlyValue.DECWRAP (fn _ => ([]
))
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( MlyValue.DECWRAP DECWRAP1, _, DECWRAP1right)) :: ( _,
 ( MlyValue.DEC DEC1, DEC1left, _)) :: rest671)) => let val  result = 
MlyValue.DECWRAP (fn _ => let val  (DEC as DEC1) = DEC1 ()
 val  (DECWRAP as DECWRAP1) = DECWRAP1 ()
 in (AST.decAdd(DEC, DECWRAP))
end)
 in ( LrTable.NT 8, ( result, DEC1left, DECWRAP1right), rest671)
end
|  ( 5, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.TYPE TYPE1, _,
 _)) :: _ :: ( _, ( MlyValue.VARLIST VARLIST1, _, _)) :: ( _, ( _, 
VAR1left, _)) :: rest671)) => let val  result = MlyValue.DEC (fn _ =>
 let val  (VARLIST as VARLIST1) = VARLIST1 ()
 val  (TYPE as TYPE1) = TYPE1 ()
 in (AST.DEC(VARLIST, TYPE))
end)
 in ( LrTable.NT 2, ( result, VAR1left, TERM1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.VARLIST (fn _ => let val  (ID as ID1) = 
ID1 ()
 in ([AST.VAR(ID)])
end)
 in ( LrTable.NT 5, ( result, ID1left, ID1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.VARLIST VARLIST1, _, VARLIST1right)) :: _ ::
 ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result
 = MlyValue.VARLIST (fn _ => let val  (ID as ID1) = ID1 ()
 val  (VARLIST as VARLIST1) = VARLIST1 ()
 in (AST.idAdd(AST.VAR(ID), VARLIST))
end)
 in ( LrTable.NT 5, ( result, ID1left, VARLIST1right), rest671)
end
|  ( 8, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.TYPE (fn _ => (AST.INT))
 in ( LrTable.NT 4, ( result, INT1left, INT1right), rest671)
end
|  ( 9, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.TYPE (fn _ => (AST.BOOL))
 in ( LrTable.NT 4, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 10, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.CMDWRAP 
CMDWRAP1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.CMDSEQ (fn _ => let val  (CMDWRAP as CMDWRAP1)
 = CMDWRAP1 ()
 in (AST.CMDSEQ(CMDWRAP))
end)
 in ( LrTable.NT 3, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 11, ( rest671)) => let val  result = MlyValue.CMDWRAP (fn _ => (
[]))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 12, ( ( _, ( MlyValue.CMDWRAP CMDWRAP1, _, CMDWRAP1right)) :: _
 :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  
result = MlyValue.CMDWRAP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  (CMDWRAP as CMDWRAP1) = CMDWRAP1 ()
 in (AST.cmdAdd(EXP, CMDWRAP))
end)
 in ( LrTable.NT 7, ( result, EXP1left, CMDWRAP1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (AST.SET(ID, EXP))
end)
 in ( LrTable.NT 6, ( result, ID1left, EXP1right), rest671)
end
|  ( 14, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.CMDSEQ 
CMDSEQ2, _, _)) :: _ :: ( _, ( MlyValue.CMDSEQ CMDSEQ1, _, _)) :: _ ::
 ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671
)) => let val  result = MlyValue.EXP (fn _ => let val  (EXP as EXP1) =
 EXP1 ()
 val  CMDSEQ1 = CMDSEQ1 ()
 val  CMDSEQ2 = CMDSEQ2 ()
 in (AST.ITE(EXP, CMDSEQ1, CMDSEQ2))
end)
 in ( LrTable.NT 6, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 15, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.CMDSEQ 
CMDSEQ1, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, 
WHILE1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _
 => let val  (EXP as EXP1) = EXP1 ()
 val  (CMDSEQ as CMDSEQ1) = CMDSEQ1 ()
 in (AST.WH(EXP, CMDSEQ))
end)
 in ( LrTable.NT 6, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.PLUS(EXP1, EXP2))
end)
 in ( LrTable.NT 6, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.MINUS(EXP1, EXP2))
end)
 in ( LrTable.NT 6, ( result, EXP1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.TIMES(EXP1, EXP2))
end)
 in ( LrTable.NT 6, ( result, EXP1left, EXP2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.DIV(EXP1, EXP2))
end)
 in ( LrTable.NT 6, ( result, EXP1left, EXP2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.MOD(EXP1, EXP2))
end)
 in ( LrTable.NT 6, ( result, EXP1left, EXP2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.LT(EXP1, EXP2))
end)
 in ( LrTable.NT 6, ( result, EXP1left, EXP2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.LEQ(EXP1, EXP2))
end)
 in ( LrTable.NT 6, ( result, EXP1left, EXP2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.EQ(EXP1, EXP2))
end)
 in ( LrTable.NT 6, ( result, EXP1left, EXP2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.GT(EXP1, EXP2))
end)
 in ( LrTable.NT 6, ( result, EXP1left, EXP2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.GEQ(EXP1, EXP2))
end)
 in ( LrTable.NT 6, ( result, EXP1left, EXP2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.NEQ(EXP1, EXP2))
end)
 in ( LrTable.NT 6, ( result, EXP1left, EXP2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.EXP (fn _ => let val  (NUM as NUM1) = 
NUM1 ()
 in (AST.NUM(NUM))
end)
 in ( LrTable.NT 6, ( result, NUM1left, NUM1right), rest671)
end
|  ( 28, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (EXP)
end)
 in ( LrTable.NT 6, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
NEGATE1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _
 => let val  (EXP as EXP1) = EXP1 ()
 in (AST.NEGATE(EXP))
end)
 in ( LrTable.NT 6, ( result, NEGATE1left, EXP1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.OR(EXP1, EXP2))
end)
 in ( LrTable.NT 6, ( result, EXP1left, EXP2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.AND(EXP1, EXP2))
end)
 in ( LrTable.NT 6, ( result, EXP1left, EXP2right), rest671)
end
|  ( 32, ( ( _, ( _, TT1left, TT1right)) :: rest671)) => let val  
result = MlyValue.EXP (fn _ => (AST.TT))
 in ( LrTable.NT 6, ( result, TT1left, TT1right), rest671)
end
|  ( 33, ( ( _, ( _, FF1left, FF1right)) :: rest671)) => let val  
result = MlyValue.EXP (fn _ => (AST.FF))
 in ( LrTable.NT 6, ( result, FF1left, FF1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 in (AST.VAREXP(ID))
end)
 in ( LrTable.NT 6, ( result, ID1left, ID1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ =>
 let val  (EXP as EXP1) = EXP1 ()
 in (AST.NOT(EXP))
end)
 in ( LrTable.NT 6, ( result, NOT1left, EXP1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : while_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun DCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun SET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun PROG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
end
end
