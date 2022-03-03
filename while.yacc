


%%

%name while

%term 
  TILDE | TERM | DCOLON | COLON | SET | COMMA | LBRACE | RBRACE | LPAREN | RPAREN | PLUS | MINUS | TIMES | DIV | MOD | PROG | VAR | INT | BOOL | IF | THEN | ENDIF | ELSE | WHILE | DO | ENDWH | TT | FF | NOT | AND | OR | NEQ | LT | LEQ | EQ | GT | GEQ | ID of string | NUM of int | EOF

%nonterm START | BLK | DEC | CMDSEQ | TYPE | VARLIST | EXP | CMDWRAP

%noshift EOF
%eop EOF
%pos int
 
%left LT LEQ EQ GT GEQ NEQ
%left TIMES DIV MOD PLUS MINUS
%right IF THEN ELSE WHILE DO
%nonassoc ENDIF ENDWH
%left OR AND
%right NOT 
%right TILDE
%right SET
%start START
%verbose


%%

START: PROG ID DCOLON BLK ()

BLK: DEC CMDSEQ ()

DEC: (*epsilon*) () 
    | VAR VARLIST COLON TYPE TERM DEC ()

VARLIST: ID () 
    | ID COMMA VARLIST () 

TYPE: INT ()
    | BOOL ()

CMDSEQ: LBRACE CMDWRAP RBRACE ()
CMDWRAP: (*epsilon*) () 
    | EXP TERM CMDWRAP ()

EXP: ID SET EXP ()
    | IF EXP THEN EXP ELSE EXP ENDIF ()
    | WHILE EXP DO EXP ENDWH ()
    | EXP PLUS  EXP ()
    | EXP MINUS EXP ()
    | EXP TIMES EXP ()
    | EXP DIV EXP ()
    | EXP MOD EXP ()
    | EXP LT EXP ()
    | EXP LEQ EXP ()
    | EXP EQ EXP ()
    | EXP GT EXP ()
    | EXP GEQ EXP ()
    | EXP NEQ EXP () 
    | NUM ()
    | LPAREN EXP RPAREN ()
    | TILDE EXP ()
    | EXP OR EXP ()
    | EXP AND EXP ()
    | TT ()
    | FF ()
    | ID ()
    | NOT EXP ()
