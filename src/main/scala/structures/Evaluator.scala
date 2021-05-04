package structures

object Evaluator {

  def evaluate(parsed: Expr): Double = {
    def loop(parsed: Expr): Expr = {
      parsed match {
        case Zero => Zero
        case Literal(_) => parsed
        case BinOp(op, expr1, expr2) => Literal(opMatch(op, expr1, expr2).toString)
        case _ => Zero
      }
    }

    loop(parsed) match {
      case Literal(a) => a.toDouble
      case _ => 0
    }
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

    def loop(exp: Expr): Expr = {
      exp match {
        case Zero => Zero
        case Literal(a) => Literal(a)
        case BinOp(op, expr1, expr2) => {
          val x = Literal(matchOp(op, loop(expr1), loop(expr2)).toString)
          x
        }

//        case BinOp(op, BinOp(c, d, e), Literal(a)) => {
//          Literal(matchOp(op, loop(BinOp(c, d, e)),
//            Literal(a)).toString)
//        }
//        case BinOp(op, Literal(a), BinOp(c, d, e)) => Literal(matchOp(op, loop(BinOp(c, d, e)),
//          Literal(a)).toString)
//        case BinOp(op, exp, Zero) => Literal(matchOp(op, loop(exp), Zero).toString)
//        case BinOp(op, Zero, exp) => Literal(matchOp(op, loop(exp), Zero).toString)
//        case BinOp(op, exp, exp2) => Literal(matchOp(op, loop(exp), loop(exp2)).toString)
      }
    }
    matchOp(op, loop(a), loop(b))
  }


}
/*
        case BinOp(op, Literal(a), Literal(b)) => Literal(opMatch(op, Literal(a), Literal(b)).toString)
        case BinOp(op, BinOp(a, b, c), Literal(d)) => Literal(opMatch(op, loop(BinOp(a, b, c)), Literal(d)).toString)
        case BinOp(op, Literal(d), BinOp(a, b, c)) => Literal(opMatch(op, loop(BinOp(a, b, c)), Literal(d)).toString)
        case BinOp(op, BinOp(a, b, c), BinOp(d, e, f)) => Literal(opMatch(op, loop(BinOp(a, b, c)),
          loop(BinOp(d, e, f))).toString)
        case BinOp(op, expr, Zero) => Literal(opMatch(op, loop(expr), Zero).toString)
        case BinOp(op, Zero, expr) => Literal(opMatch(op, loop(expr), Zero).toString)
        case _ => Zero
 */