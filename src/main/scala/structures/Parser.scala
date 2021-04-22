package structures

import scala.annotation.tailrec

sealed trait Expr
case class BinOp(op: String, arg1: Expr, arg2: Expr) extends Expr
case class Literal(value: String) extends Expr
case object Empty extends Expr

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

//  def createSyntax(l: List[Lexed]): Tree[String] = {
//
//    def loop(pFound: Boolean, exprs: List[Expr], i: Int): List[Expr] = (l(i),l(i+1)) match {
//      case (h1, h2) if h2._2.classifier=="o_parenthesis" && !pFound => {
//        val lastPIndex = parenthesesLastIndex(i, l)
//
//        loop(true, exprs :+ , i+1)
//        ???
//      }
//    }
//    ???
//  }

////  // ((5*3)*3)*2)  -> List((*, 2, BinOp(*, 3, BinOp(*, 3, 5))), ...)
//    // List( ('(', open), (5, num), (*, r1), (3, num), ( ')', close))
//    // ((5*3)*2)) - 5 - 5 => 20
      // (30) + -5 + -5 => 20 how would the above be ordered?
      // List( ((, open)), ((, open)), (5, num), (*, r1), (3, num), (), close), (*, r1), (2, num), (), close),
  //  (), close), (-, r2), (5, num))
      // List( BinOp(*, 2, BinOp(*, 3, 5)), (-, 0, 5), (-, 0, 5))
      // List( Lit(30), Lit(-5), Lit(-5))
      // Lit30 + Lit-5 + Lit-5 = 20


    // (5*2) - 52 - 3 - 4
    // ((, open), (5, num), (*, r1), (2, num), (), close), (-, r2), (52, num) (-, r2), (3, num), (-, r2), (4, num))
    // List( BinOp(*, 5, 2), (-, 0, 52), (-, 0, 3), (-, 0, 4) )
    // 10 + -52 + -3 + -4 => -50
// def assembleExprs(subLexedList: List[Lexed], lastPIndex: Int): List[Expr] = {
//
//    def loop(exprs: List[Expr], i: Int): List[Expr] = {
//      (subLexedList(i), subLexedList(i-1)) match {
//        case (numLexed, opLexed) => {
//          val curNumber = numLexed._1
//          val curOp = opLexed._1
//          val outermostExpr =
//        }
//      }
//
//    }
//
//
  //
  //
//  }

  // ((5*3)/3) i=8
  // i-2>0 BinOp(/, loop(l.drop2, i-2), 3)
  // i=5 - 2 > 0 => BinOp(*, 5, 3)
  // BinOp(/, BinOp(*, 5, 3), 3)


  def pPrint[A](obj: List[A]): Unit = {

    for(i <- 0 to obj.size-1) {
      print(obj(i)+ " | ")
    }

  }

  def shedOffNum(sublist: List[Lexed]): Int = {
    val token = sublist(sublist.size-1)._2.classifier
    if(token==")" || token=="(")
      shedOffNum(sublist.dropRight(1))
    sublist.size-1
  }

  // (, 5, *, 4, )
  def gatherInnrExprs(sublist: List[Lexed], opening: Int): Expr = {
    println(sublist)
    def loop(l: List[Lexed], i: Int, closed: Int): Expr = {
      if(i-2>0 && closed < opening) {

        val (l2, l1) = (l(i-2), (l(i-1)))
        val (token2, token1) = (l2._2, l1._2)
        val (op, arg2) = (l2._1, l1._1)

        val shed = l.size - shedOffNum(l)
        println(s"$shed shedOff ${shedOffNum(l)}")
        BinOp(op, loop(l.dropRight(shed), shedOffNum(l), closed+1), Literal(arg2)) // loop arguments obviously don't work

      }
      else {
        Empty
      }
    }

    loop(sublist, sublist.size-1, 0)
  }

  def parenthesesLastIndex(start: Int, l: List[Lexed]): (Int, Int) = {
    val max = l.size
    var encounteredOpen = 0

    for( i <- start to max ) {
      val curLexed: Lexed = l(i)
      val curToken = curLexed._2.classifier
      if(curToken!="(")
        return (encounteredOpen, i)
      else
        encounteredOpen+=1
    }
    (encounteredOpen, max)
  }


}
