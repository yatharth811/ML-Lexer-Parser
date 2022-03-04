


%%

%name while

%term 
  NEGATE | TERM | DCOLON | COLON | SET | COMMA | LBRACE | RBRACE | LPAREN | RPAREN | PLUS | MINUS | TIMES | DIV | MOD | PROG | VAR | INT | BOOL | IF | THEN | ENDIF | ELSE | WHILE | DO | ENDWH | TT | FF | NOT | AND | OR | NEQ | LT | LEQ | EQ | GT | GEQ | ID of string | NUM of int | EOF

%nonterm START | BLK | DEC | CMDSEQ | TYPE | VARLIST | EXP | CMDWRAP | DECWRAP | DECSEQ | ITERM | IEXP | IFACTOR | BEXP | BTERM | BFACTOR | CMD

%noshift EOF
%eop EOF
%pos int
%right DO
%nonassoc THEN SET NUM
%nonassoc ELSE
%left COMMA
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIV MOD
%right NEGATE NOT

%start START
%verbose


%%

START: PROG ID DCOLON BLK                   ()

BLK: DECSEQ CMDSEQ                          ()

DECSEQ: DECWRAP                             ()
DECWRAP: (*epsilon*)                        () 
    | DEC DECWRAP                           ()

DEC: VAR VARLIST COLON TYPE TERM            ()

VARLIST: ID                                 () 
    | ID COMMA VARLIST                      () 

TYPE: INT                                   ()
    | BOOL                                  ()

CMDSEQ: LBRACE CMDWRAP RBRACE               ()
CMDWRAP: (*epsilon*)                        () 
    | CMD TERM CMDWRAP                      ()

CMD: ID SET EXP                             ()
    | IF BEXP THEN CMDSEQ ELSE CMDSEQ ENDIF ()
    | WHILE BEXP DO CMDSEQ ENDWH            ()

EXP: IEXP                                   ()
    | BEXP                                  ()
IEXP: IEXP PLUS ITERM                       ()
    | IEXP MINUS ITERM                      ()
    | ITERM                                 ()

ITERM: ITERM TIMES IFACTOR                  ()
    | ITERM DIV IFACTOR                     ()
    | ITERM MOD IFACTOR                     ()
    | IFACTOR                               ()


IFACTOR: NUM                                ()
    | ID                                    ()
    | LPAREN IEXP RPAREN                    ()
    | NEGATE IFACTOR                        ()

BEXP: BEXP OR BTERM                         ()
    | BTERM                                 ()

BTERM: BTERM AND BFACTOR                    ()
    | BFACTOR                               ()

BFACTOR: TT                                 ()
    | FF                                    ()
    | ID                                    ()
    | IEXP LT IEXP                          ()
    | IEXP LEQ IEXP                         ()
    | IEXP EQ IEXP                          ()
    | IEXP GT IEXP                          ()
    | IEXP GEQ IEXP                         ()
    | IEXP NEQ IEXP                         ()
        