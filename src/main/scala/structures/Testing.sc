import structures.{Lexer, Parser}

val lexer = new Lexer
val parser = new Parser
val sampleExpr = "5*4*2+3"
val lexed = lexer.lex(sampleExpr)




