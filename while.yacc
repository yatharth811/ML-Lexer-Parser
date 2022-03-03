


%%

%name while

%term 
  NEGATE | TERM | DCOLON | COLON | SET | COMMA | LBRACE | RBRACE | LPAREN | RPAREN | PLUS | MINUS | TIMES | DIV | MOD | PROG | VAR | INT | BOOL | IF | THEN | ENDIF | ELSE | WHILE | DO | ENDWH | TT | FF | NOT | AND | OR | NEQ | LT | LEQ | EQ | GT | GEQ | ID of string | NUM of int | EOF

%nonterm START of AST.PROG | BLK of AST.BLK | DEC of AST.DEC | CMDSEQ of AST.CMDSEQ | TYPE of AST.TYP | VARLIST of AST.IDSEQ | EXP of AST.EXP | CMDWRAP of AST.CMDSEQ | DECSEQ of AST.DECSEQ

%noshift EOF
%eop EOF
%pos int
 
%left LT LEQ EQ GT GEQ NEQ
%left TIMES DIV MOD PLUS MINUS
%right IF THEN ELSE WHILE DO
%nonassoc ENDIF ENDWH
%left OR AND
%right NOT 
%right NEGATE
%right SET
%start START
%verbose


%%

START: PROG ID DCOLON BLK                   (AST.PROG(ID, BLK))

BLK: DECSEQ CMDSEQ                          (AST.BLK(DECSEQ, CMDSEQ))

DECSEQ: (*epsilon*)                         (AST.DECS) 
    | DEC DECSEQ                            (AST.DECSEQ(DEC, DECSEQ))

DEC: VAR VARLIST COLON TYPE TERM            (AST.DEC(VARLIST, TYPE))

VARLIST: ID                                 (AST.IDS(ID)) 
    | ID COMMA VARLIST                      (AST.IDSEQ(ID, VARLIST)) 

TYPE: INT                                   (AST.INT)
    | BOOL                                  (AST.BOOL)

CMDSEQ: LBRACE CMDWRAP RBRACE               (CMDWRAP)
CMDWRAP: (*epsilon*)                        (AST.CMDS) 
    | EXP TERM CMDWRAP                      (AST.CMDSEQ(EXP, CMDWRAP))

EXP: ID SET EXP                             (AST.SET(ID, EXP))
    | IF EXP THEN CMDSEQ ELSE CMDSEQ ENDIF  (AST.ITE(EXP, CMDSEQ1, CMDSEQ2))
    | WHILE EXP DO CMDSEQ ENDWH             (AST.WH(EXP, CMDSEQ))
    | EXP PLUS  EXP                         (AST.PLUS(EXP1, EXP2))
    | EXP MINUS EXP                         (AST.MINUS(EXP1, EXP2))
    | EXP TIMES EXP                         (AST.TIMES(EXP1, EXP2))
    | EXP DIV EXP                           (AST.DIV(EXP1, EXP2))
    | EXP MOD EXP                           (AST.MOD(EXP1, EXP2))
    | EXP LT EXP                            (AST.LT(EXP1, EXP2))
    | EXP LEQ EXP                           (AST.LEQ(EXP1, EXP2))
    | EXP EQ EXP                            (AST.EQ(EXP1, EXP2))
    | EXP GT EXP                            (AST.GT(EXP1, EXP2))
    | EXP GEQ EXP                           (AST.GEQ(EXP1, EXP2))
    | EXP NEQ EXP                           (AST.NEQ(EXP1, EXP2)) 
    | NUM                                   (AST.NUM(NUM))
    | LPAREN EXP RPAREN                     (EXP)
    | NEGATE EXP                            (AST.NEGATE(EXP))
    | EXP OR EXP                            (AST.OR(EXP1, EXP2))
    | EXP AND EXP                           (AST.AND(EXP1, EXP2))
    | TT                                    (AST.TT)
    | FF                                    (AST.FF)
    | ID                                    (AST.ID)
    | NOT EXP                               (AST.NOT(EXP))
