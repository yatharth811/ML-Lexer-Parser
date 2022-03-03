exception syntaxErr
structure whileLrVals = whileLrValsFun(structure Token = LrParser.Token)
structure whileLex = whileLexFun(structure Tokens = whileLrVals.Tokens);
structure whileParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = whileLrVals.ParserData
     	       structure Lex = whileLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,_) =
		    	( TextIO.output(TextIO.stdOut, "Syntax Error:"^Int.toString(pos)^":"^Int.toString(pos)^":"^s) ; raise syntaxErr )
		in
		    whileParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  whileParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = whileLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = whileParser.Stream.get lexer
    in
        if whileParser.sameToken(nextToken, dummyEOF) then (result)
 		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
		
    end

fun read (infile:string) =
   let 
        val instream = TextIO.openIn infile
	fun loop instream =
	    String.implode(String.explode(TextIO.inputAll instream))

    in
	    loop instream before TextIO.closeIn instream
    end

val parseString = parse o stringToLexer 
val parseFile = parse o stringToLexer o read 