
val _ = Control.Print.printDepth := 10;
val _ = Control.Print.printLength := 100;

structure LangLrVals : Lang_LRVALS = 
  LangLrValsFun (structure Token = LrParser.Token)

structure LangLex : LEXER =
  Lang_LexFun (structure Tokens = LangLrVals.Tokens)

structure LangParser : PARSER =
  Join (structure ParserData = LangLrVals.ParserData
        structure Lex = LangLex
        structure LrParser = LrParser)

structure Parser =
struct
  fun parse (filename: string) = 
    let
      val file = TextIO.openIn (filename)
      val lexer = LangParser.makeLexer (fn _ => case TextIO.inputLine file
        of SOME s => s
         | _ => "")
      (* Column information is not accurate *)
      fun printError (msg: string, line: int, col: int) = 
        print ("Parsing error at line " ^ (Int.toString line) ^
               ": " ^ msg ^ "\n")
      fun loop lexer = let
        val (res, lexer) = LangParser.parse (100, lexer, printError, ())
        val (next, lexer) = LangParser.Stream.get lexer
      in
        if LangParser.sameToken (next, LangLrVals.Tokens.EOF(~1,~1)) then res
        else loop lexer
      end
    in
      loop lexer
    end
end

