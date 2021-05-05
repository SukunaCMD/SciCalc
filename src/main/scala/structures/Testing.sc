import structures.{Lexer, Parser}

val lexer = new Lexer
val sampleExpr = "4+35*4*2+3"







lexer.buildParens(sampleExpr, 4)

lexer.nextCloseParen(sampleExpr, 0)















