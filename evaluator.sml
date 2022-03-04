structure EVALUATOR = 
struct 
open AST
val brokenTypes = Fail "Error in Evaluation!"

fun convertType (typeInput) : TYPEVAL = 
    if typeInput = INT then INTTYPE else BOOLTYPE

fun printFunctionType(input : TYPEVAL) : string = 
    if input = INTTYPE then "int" else "bool"

(* fun checkExpression(ex : EXP, env: TYPEENV) = 
    case ex of
        NUM nm                  => INTTYPE
        | PLUS (exp1, exp2)     => checkPlus(exp1, exp2)
        | MINUS (exp1, exp2)    => checkMinus(exp1, exp2)
        | TIMES (exp1, exp2)    => checkTimes(exp1, exp2)
        | DIV (exp1, exp2)      => checkDiv(exp1, exp2)
        | MOD (exp1, exp2)      => checkMod(exp1, exp2)
        | LT (exp1, exp2)       => checkLt(exp1, exp2)
        | LEQ (exp1, exp2)      => checkLeq(exp1, exp2)
        | EQ (exp1, exp2)       => checkEq(exp1, exp2)
        | GT (exp1, exp2)       => check
        | GEQ (exp1, exp2)      =>
        | NEQ (exp1, exp2)      => *)


end
