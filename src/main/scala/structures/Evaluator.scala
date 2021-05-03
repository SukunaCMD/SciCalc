package structures

object Evaluator {

  def eval(parsed: Expr): Double = {
    def loop(parsed: Expr, acc: Double): Double = parsed match {
      case Literal(a) => {
        a.toDouble
      }
      case BinOp(op, Literal(a), Literal(b)) => {
        traverseOperation(BinOp(op, Literal(a), Literal(b)))
      }
      case BinOp(op, BinOp(cur, arg1, arg2), Literal(b)) => {
        loop(BinOp(cur, arg1, arg2), 0) + traverseOperation(BinOp(op, Zero, Literal(b)))
      }
      case BinOp(op, Literal(b), BinOp(cur, arg1, arg2)) => {
        loop(BinOp(cur, arg1, arg2), 0) + traverseOperation(BinOp(op, Zero, Literal(b)))
      }
      case BinOp(op, Literal(num), _) => {
        traverseOperation(BinOp(op, Literal(num), Zero))
      }
      case BinOp(op, _, Literal(b)) => {
        traverseOperation(BinOp(op, Zero, Literal(b)))
      }
      case BinOp(_, BinOp(cur, argA, argB), BinOp(cur2, arg2a, arg2b)) => {
        loop(BinOp(cur, argA, argB), 0) +
          loop(BinOp(cur2, arg2a, arg2b), 0)
      }
      case BinOp(_, BinOp(cur, argA, argB), _) => {
        traverseOperation(BinOp(cur, argA, argB))
      }
      case BinOp(_, _, BinOp(cur, argA, argB)) => {
        traverseOperation(BinOp(cur, argA, argB))
      }
      case _ => 0.0
    }
    loop(parsed, 0)
  }

  def traverseOperation(a: Expr): Double = {
    def loop(inner: Expr): Double = inner match {
      case Literal(a) => a.toDouble
      case BinOp(cur, Literal(a), Literal(b)) => opMatch(cur, Literal(a), Literal(b))
      case BinOp(cur, binOp1, binOp2) => opMatch(cur, binOp1, binOp2)
      case BinOp(cur, binOp1, Zero) => opMatch(cur, binOp1, Zero)
      case BinOp(cur, Zero, binOp1) => opMatch(cur, binOp1, Zero)
      case _ => 0.0
    }
    loop(a)
  }



  def opMatch(op: String, a: Expr, b: Expr): Double = {
    def matchOp(op: String, a: Expr, b: Expr): Double = {
      op match {
        case "+" => Arithmetic.add(a, b)
        case "-" => Arithmetic.subtract(a, b)
        case "*" => Arithmetic.multiply(a, b)
        case "/" => Arithmetic.divide(a, b)
      }
    }

    def loop(exp: Expr): Literal = exp match {
      case Zero => Literal("0")
      case Literal(a) => Literal(a)
      case BinOp(op, exp, Zero) => Literal(matchOp(op, loop(exp), Zero).toString)
      case BinOp(op, Zero, exp) => Literal(matchOp(op, loop(exp), Zero).toString)
      case BinOp(op, exp, exp2) => Literal(matchOp(op, loop(exp), loop(exp2)).toString)
    }

    matchOp(op, loop(a), loop(b))
  }


}
