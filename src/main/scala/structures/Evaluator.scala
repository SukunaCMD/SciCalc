package structures

class Evaluator {

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

  def unitTestInnerExprs: Unit = {
    val sampleText = "5*4"
    val parser = new Parser
    val lexer = new Lexer
    val parenthesized = lexer.parenthesize(lexer.lex(sampleText))
    println(parenthesized)
    val lexed = lexer.lex2(parenthesized)
    val innrExprs = parser.gatherInnrExprs(lexed, 2)

    //println(innrExprs)


  }

  def main(args: Array[String]): Unit = {
    val testLexer = new Lexer
    val sample = "5*32+43"
    val parser = new Parser

    unitTestInnerExprs

  }

  def doIDoAnything: Unit = ???
}