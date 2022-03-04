# Instructions
- run `make all` in command prompt. SML window will be opened.
- now simply run `parseFile "<FileName>"`. All tokens will be printed along with parse tree for that file. (Note: File should be in same directory)
- use `make clean` to remove extra files when you're done.  
# Context-Free Grammar
```
Program         -> "program"  Identifier "::" Block
Block           -> DeclarationSeq CommandSeq
DeclarationSeq  -> ϵ 
                 | Declaration DeclarationSeq
Declaration     -> "var" VariableList ":" Type ";"
Type            -> "int"
                 | "bool"
VariableList    -> Identifier
                 | Identifier "," VariableList
CommandSeq      -> "{" CommandList "}"
CommandList     -> ϵ
                 | Command ";" CommandList
Command         -> Identifier ":=" Expression
                 | "read" Identifier
                 | "write" Expression
                 | "if" Expression "then" CommandSeq "else" CommandSeq "endif"
                 | "while" Expression "do" CommandSeq "endwh"
Expression      -> Expression "+" Expression
                 | Expression "-" Expression
                 | Expression "*" Expression
                 | Expression "/" Expression
                 | Expression "%" Expression
                 | Expression "<" Expression
                 | Expression "<=" Expression
                 | Expression "=" Expression
                 | Expression ">" Expression
                 | Expression ">=" Expression
                 | Expression "<>" Expression
                 | Num
                 | "(" Expression ")"
                 | "~" Expression
                 | Expression "||" Expression
                 | Expression "&&" Expression
                 | "tt"
                 | "ff"
                 | Identifier
                 | "!" Expression

Identifier      ->  Character+ Digit*
Num             ->  Digit1 Digit*
Digit           ->  0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
Digit1          ->  1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
Character       ->  “A” | “B” | “C” | “D” | “E” | “F” | “G” | “H” 
                    | “I” | “J” | “K” | “L” | “M” | “N” | “O” | “P”
                    | “Q” | “R” | “S” | “T” | “U” | “V ” | “W” | “X”
                    | “Y ” | “Z” | “a” | “b” | “c” | “d” | “e” | “f”
                    | “g” | “h” | “i” | “j” | “k” | “l” | “m” | “n” 
                    | “o” | “p” | “q” | “r” | “s” | “t” | “u” | “v”
                    | “w” | “x” | “y” | “z”

```

# AST Datatype Definition
```
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

end
```
# Syntax Directed Translation
```
Program         -> "program"  Identifier "::" Block                             (PROG(Identifier, Block))
Block           -> DeclarationSeq CommandSeq                                    (BLK(DeclarationSeq, CommandSeq))
DeclarationSeq  -> ϵ                                                            ([]) 
                 | Declaration DeclarationSeq                                   (Declaration :: DeclarationSeq)
Declaration     -> "var" VariableList ":" Type ";"                              (DEC(VariableList, Type))
Type            -> "int"                                                        (Type.val = "int")
                 | "bool"                                                       (Type.val = "bool")
VariableList    -> Identifier                                                   ([Identifier])
                 | Identifier "," VariableList                                  (Identifier :: VariableList)
CommandSeq      -> "{" CommandList "}"                                          (CommandList)
CommandList     -> ϵ                                                            ([])
                 | Command ";" CommandList                                      (Command :: CommandList)
Command         -> Identifier ":=" Expression                                   (SET(Identifier, Expression))
                 | "read" Identifier                                            (Command.val = READ(Identifier))
                 | "write" Expression                                           (Command.val = WRITE(Expression))
                 | "if" Expression "then" CommandSeq "else" CommandSeq "endif"  (ITE(Expression, CommandSeq, CommandSeq))
                 | "while" Expression "do" CommandSeq "endwh"                   (WH(Expression, CommandSeq, CommandSeq))
Expression      -> Expression "+" Expression                                    (E.val = E1.val + E2.val)
                 | Expression "-" Expression                                    (E.val = E1.val - E2.val)
                 | Expression "*" Expression                                    (E.val = E1.val * E2.val)
                 | Expression "/" Expression                                    (E.val = E1.val / E2.val)
                 | Expression "%" Expression                                    (E.val = E1.val % E2.val)
                 | Expression "<" Expression                                    (E.val = E1.val < E2.val)
                 | Expression "<=" Expression                                   (E.val = E1.val <= E2.val)
                 | Expression "=" Expression                                    (E.val = E1.val = E2.val)
                 | Expression ">" Expression                                    (E.val = E1.val > E2.val)
                 | Expression ">=" Expression                                   (E.val = E1.val >= E2.val)
                 | Expression "<>" Expression                                   (E.val = E1.val <> E2.val)
                 | Num                                                          (E.val = Num.val)
                 | "(" Expression ")"                                           (E.val = E1.val)
                 | "~" Expression                                               (E.val = NEGATE(E1.val))
                 | Expression "||" Expression                                   (E.val = E1.val OR E2.val)
                 | Expression "&&" Expression                                   (E.val = E1.val AND E2.val)
                 | "tt"                                                         (E.val = TRUE)
                 | "ff"                                                         (E.val = FALSE)
                 | Identifier                                                   (E.val = Identifier.val)
                 | "!" Expression                                               (E.val = NOT(E1.val))
Identifier      ->  Character+ Digit*
Num             ->  Digit1 Digit*
Digit           ->  0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
Digit1          ->  1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
Character       ->  “A” | “B” | “C” | “D” | “E” | “F” | “G” | “H” 
                    | “I” | “J” | “K” | “L” | “M” | “N” | “O” | “P”
                    | “Q” | “R” | “S” | “T” | “U” | “V ” | “W” | “X”
                    | “Y ” | “Z” | “a” | “b” | “c” | “d” | “e” | “f”
                    | “g” | “h” | “i” | “j” | “k” | “l” | “m” | “n” 
                    | “o” | “p” | “q” | “r” | “s” | “t” | “u” | “v”
                    | “w” | “x” | “y” | “z”


```

# Other Design & Implementation Decisions
- Merged IntTerm, IntFactor, BoolTerm, BoolFactor into Expression to avoid reduce-reduce conflicts in parser. 

# Acknowledgements
- Boilerplate Code for load-while.sml referred from http://cs.wellesley.edu/~cs235/fall08/lectures/35_YACC_revised.pdf.