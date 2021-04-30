package structures

class Evaluator {

  def evaluateListExpr(l: List[Expr]): Double = {

    0.0
  }

  def evaluate(parsed: Tree[String]): Double = {

    def loop(parsed: Tree[String]): Double = parsed match {
      case Leaf(num) => num.toDouble
      case Branch(cur, l, r) => {
        println(cur)
        operate(l, cur.toDouble, loop(r))
      }
    }
    loop(parsed)
  }

  def operate(operator: Tree[String], arg1: Double, arg2: Double): Double = operator match {
    case Leaf("+") => arg1+arg2
    case Leaf("-") => arg1-arg2
    case Leaf("/") => arg1/arg2
    case Leaf("*") => arg1*arg2
  }

}
object Main {


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

  def testParser: Unit = {
    val sampleText = "5*2+3+4*5+1"
    val parser = new Parser
    val lexer = new Lexer
    val lexed = lexer.lex(sampleText)
    val test = parser.parse(lexed)


    println()
    println(test)
  }

  def testWhileCoding: Unit = {
    val firstBranch = Branch("+", Leaf("1"), Leaf("2"))
    val secondBranch = Branch("*", Leaf("3"), Leaf("4"))
    val thirdBranch = Branch("+", firstBranch, Leaf("6"))
    val ultimate = Branch("sent", secondBranch, thirdBranch)

    val printed = Tree.pPrint(ultimate)
    println(printed)
    println(ultimate)
  }

  def main(args: Array[String]): Unit = {
    testWhileCoding
  }
}