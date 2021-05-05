import structures.{Lexer, Parser}

val lexer = new Lexer
val sampleExpr = "5*4*2+3"



lexer.buildParens(sampleExpr, 0)

lexer.nextCloseParen(sampleExpr, 0)















