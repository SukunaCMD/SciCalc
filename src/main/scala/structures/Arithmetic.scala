package structures

object Arithmetic {


  def add(a: Expr, b: Expr): Double = {
    (a, b) match {
      case (Literal(x), Literal(y)) => x.toDouble+y.toDouble
      case _ => 0.0
    }
  }

  def subtract(a: Expr, b: Expr): Double = {
    (a, b) match {
      case (Literal(x), Literal(y)) => x.toDouble-y.toDouble
      case (Zero, Literal(y)) => 0-y.toDouble
      case (Literal(x), Zero) => x.toDouble
      case _ => 0.0
    }
  }

  def multiply(a: Expr, b: Expr): Double = {
    (a, b) match {
      case (Literal(x), Literal(y)) => x.toDouble*y.toDouble
      case (BinOp(op, arg1, arg2), Literal(y)) => {
        ???
      }
      case _ => 0.0
    }
  }

  def divide(a: Expr, b: Expr): Double = {
    (a, b) match {
      case (Literal(x), Literal(y)) => x.toDouble/y.toDouble
      case (BinOp(op, arg1, arg2), Literal(y)) => {
        ???
      }
      case _ => 0.0
    }
  }

}
