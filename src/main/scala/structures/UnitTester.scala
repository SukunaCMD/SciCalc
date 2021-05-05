package structures

import structures.UnitTester.testWhileCoding

object UnitTester {

  def arrayedSamples: Array[String] =
    Array("2-5*75/5/2")

  def lexerTests: Unit = {
    val sample1 = "5/4*2+3-3"
    val sample2 = "5/4*2-3-3"
    val sample3 = "5/4*2-3*3+3"

    val tests = Array(sample3, sample2, sample1)
    val lexer = new Lexer

    for(i <- 0 to tests.size-1) {
      val lexed = lexer.lex(tests(i))
      println(s"Lexed cur: $lexed")
      println()
    }
  }

  def unitTestInnerExprs: Unit = {
    val sampleText = "2+3-3+4-4+8" // 2+4-4 => 2+8 = 10
    val parser = new Parser
    val lexer = new Lexer
    val lexed = lexer.lex(sampleText)

    println()


  }

  def testParser(text: String): Unit = {

    val parser = new Parser
    val lexer = new Lexer
    val lexed = lexer.lex(text)
    val test = parser.parse(lexed)


    println()
    println(test)
  }

  def testPrinter(text: String): Unit = {
    val parser = new Parser
    val lexer = new Lexer
    val lexed = lexer.lex(text)
    val test = parser.parse(lexed)

    println(test)
    println("")
    val pretty = Expr.pPrint(test)
    println(pretty)
  }

  def testNewTree: Unit = {
    val firstBranch = Branch("+", EmptyTree, Leaf("2"))
    val secondBranch = Branch("*", Leaf("3"), Leaf("4"))
    val thirdBranch = Branch("+", firstBranch, Leaf("6"))
    val ultimate = Branch("sent", secondBranch, thirdBranch)

    val printed = Tree.pPrint(ultimate)
    println(printed)
    println(ultimate)
  }

  def testEvaluator(text: String): Unit = {
    val parser = new Parser
    val lexer = new Lexer
    val lexed = lexer.lex(text)
    val test = parser.parse(lexed)

    val added = Evaluator.evaluate(test)
    println(s"$added added")

  }

  def testParenthesize(text: String): Unit = {
    val lexer = new Lexer
    lexer.buildParens(text, 0)
    println(lexer.buildParens(text, 0))
  }

  def testWhileCoding: Unit = {
    val lexer = new Lexer
    println(lexer.isR2('+'))
  }
}
object Main {

  def main(args: Array[String]): Unit = {
    val samples = UnitTester.arrayedSamples

    for( i <- 0 to samples.size-1) {
      UnitTester.testParenthesize(samples(i))
    }

  }

}