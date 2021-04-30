package structures

import scala.annotation.tailrec

sealed trait Expr
case class BinOp(op: String, arg1: Expr, arg2: Expr) extends Expr
case class Literal(value: String) extends Expr
case object zero extends Expr

class Parser {

  type Lexed = (String, Token)


  def createTree(tokens: List[Lexed]): Tree[String] = {

    def loop(l: List[Lexed]): Tree[String] = l match {
      case h1 :: h2 :: tail => Branch(h1._1,Leaf(h2._1), loop(tail))
      case h1  :: h2 :: Nil => Branch(h1._1, Leaf(h2._1), loop(h2 :: Nil))
      case last :: Nil => Leaf(last._1)
    }
    loop(tokens)
  }

  // ((5*3)/2) + 4
  def parse(tokens: List[Lexed]): Expr = {

    def loop(l: List[Lexed]): Expr = l match {
      case op :: paren :: tail if ( isR2Op(op) && isParenthesis(paren)) => {
        val excludedInner = excludeInners(paren :: tail)
        val excludedOuter = excludeOuters(paren::tail)
        val inners = gatherInnerExpr(excludedOuter)
        BinOp(op._1, inners, loop(excludedInner))
      }
      case h1 :: _ :: tail if isParenthesis(h1) => {
        BinOp("+", gatherInnerExpr(excludeOuters(l)), loop(excludeInners(l))) // uses the parameter l here b/c h1="("
      }
      case op :: p :: tail if isParenthesis(p) => {
        val excludedInner = excludeInners(p :: tail)
        println(s"${gatherInnerExpr(excludeOuters(l))} hello boys")
        BinOp(op._1, gatherInnerExpr(excludeOuters(l)), loop(excludedInner))
      }
      case op :: num :: tail => {
        BinOp(op._1, Literal(num._1), loop(tail))
      }
      case h1  :: h2 :: Nil => BinOp(h1._1, zero, Literal(h2._1))
      case last :: Nil => Literal(last._1)
      case _ => zero
    }
    loop(tokens)
  }

  // ((5*4)*2) + (3*3)
  // returns list of tokens after done with gathering inner expr

  def excludeInners(l: List[Lexed]): List[Lexed] = {
    def loop(l: List[Lexed], encountered: Int): List[Lexed] = l match {
      case _ if encountered<1 => l
      case h :: t if h._1=="(" => loop(t, encountered+1)
      case h :: t if h._1==")" => loop(t, encountered-1)
      case _ :: t => loop(t, encountered)
    }
    loop(l.tail, 1)
  }

  def excludeOuters(l: List[Lexed]): List[Lexed] = {
    def loop(l: List[Lexed], encountered: Int, build: List[Lexed]): List[Lexed] = {
      l match {
        case _ if encountered < 1 => build
        case h :: t if h._1 == "(" => loop(t, encountered + 1, build)
        case h :: t if h._1 == ")" => loop(t, encountered - 1, build)
        case h :: t => loop(t, encountered, build :+ h)
        case Nil => Nil // wat do here
      }
    }
    loop(l.tail, 1, List(l.head))
  }

  def shedOffNum(sublist: List[Lexed]): Int = {
    val token = sublist(sublist.size-1)._2.classifier
    if(token==")" || token=="(")
      shedOffNum(sublist.dropRight(1))
    sublist.size-1
  }

  def gatherInnerExpr(sublist: List[Lexed]): Expr = {
    val filteredPs = sublist.filter{
      (lexed: Lexed) =>
        (lexed._2.classifier!="c_parenthesis" && lexed._2.classifier!="o_parenthesis")
    }

    def loop(l: List[Lexed]): Expr = {
      l match {
        case h :: _ if isParenthesis(h) => zero
        case h :: h2 :: tail => {
          val (num, op) = (h._1, h2._1)

          BinOp(op, loop(tail), Literal(num))
        }
        case h :: Nil => {
          Literal(h._1)
        }
        case _ => zero // this shouldn't run i think
      }
    }
    loop(filteredPs.reverse)
  }


  def parenthesesLastIndex(start: Int, l: List[Lexed]): Int = {
    val max = l.size
    var encountered = 0
// ((5*4)*2)+(3*3)
    for( i <- start to max-1 ) {
      val curLexed: Lexed = l(i)
      val curToken = curLexed._2.classifier
      if(curToken=="c_parenthesis" && encountered>0) {
        encountered -= 1
      }
      else if(curToken=="o_parenthesis") {
        encountered += 1
      }
      if(curToken=="c_parenthesis" && encountered==0) {
        return i
      }
    }
    max
  }

  def isParenthesis(lexed: Lexed): Boolean = lexed._2 match {
    case cur if cur.classifier=="c_parenthesis" => true
    case cur if cur.classifier=="o_parenthesis" => true
    case _ => false
  }

  def isR2Op(lexed: Lexed): Boolean = lexed._2 match {
    case cur if cur.classifier=="r2" => true
    case _ => false
  }
}
/*
  // ((5/4)*2)+(3*3) + 5
  // List(BOp(*, BOp(/, 5, 4), 2), BOp(*, 3, 3)) v
  def parseList(l: List[Lexed]): List[Expr] = {
    // ((5*4)*2) 8 is the last index
    def loop(exprs: List[Expr], lexedList: List[Lexed], i: Int): List[Expr] = lexedList match {
      case Nil => exprs
      case h1 :: h2 :: h3 :: tail if (isR2Op(h1) && (!isParenthesis(h2)) ) => {
        val (num, op) = (h2._1, h1._1)
        val newBinOp = BinOp(op, Literal("0"), Literal(num)) // must carry over?
        loop(exprs:+newBinOp, tail, i)
      }
      case op :: paren :: h3 :: tail if (isR2Op(op) && isParenthesis(paren) ) => {
        val lastPIndex = parenthesesLastIndex(i, lexedList)
        val subList = lexedList.slice(i, lastPIndex+1)// excludes the second parameter
        println(s"Correct: (, 4, *, 5, )  cur: ${subList}")
        val innerExprs = gatherInnerExprs(subList)
        ???
      }

      // + 3 + 3
//      case h1 :: h2 :: h3 :: tail if (isR2Op(h1) && (!isParenthesis(h2)) ) => {
//        val (num, op) = (h2._1, h1._1)
//        loop(exprs:+ BinOp(op, Literal("0"), Literal(num)), tail, i)
//        ???
//
//      }
      case h1 :: h2 :: h3 :: tail => {
        val (firstNum, operator, secondNum) = (h1._1, h2._1, h3._1)
        loop(exprs:+BinOp(operator, Literal(firstNum), Literal(secondNum)), tail, i)
      }
      // 2+3*3+4+8
      case h1 :: h2 :: Nil => {
        exprs:+ BinOp(h1._1, Literal("0"), Literal(h2._1))
      }
      case h1 :: Nil => {
        exprs:+Literal(h1._1)
      }
    }
    loop(List[Expr](), l, 0)
  }

    // assembles inner expressions within a sublist, does not actually formulate sublist though.
  // delegate that task to another fn
  def gatherInnerExprs(sublist: List[Lexed]): Expr = {
    val filteredPs = sublist.filter{
      (lexed: Lexed) =>
        (lexed._2.classifier!="c_parenthesis" && lexed._2.classifier!="o_parenthesis")
    }
    def loop(l: List[Lexed], i: Int): Expr = {
      if(i-1<0 && i>=0) {
        Literal(l(i)._1)
      }
      else if(i-1<0) {

        zero
      }else{
        val (operator, number) = (l(i - 1)._1, l(i)._1)

        BinOp(operator, loop(l, i - 2), Literal(number))
      }
    }
    loop(filteredPs, filteredPs.size-1)
  }


 */