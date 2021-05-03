package structures

trait Expr
case class BinOp(op: String, arg1: Expr, arg2: Expr) extends Expr
case class Literal(value: String) extends Expr
case object Zero extends Expr


object Expr {

  def pPrint[A](t: Expr): String = {
    def loop(t: Expr, buildUp: String, padding: String, pointer: String): String = t match {
      case BinOp(cur, left, right) => {
        val newBuildUp = buildUp + padding + pointer + cur + "\n"
        val newPadding = padding + "│  "
        val newPointerRight = "└──"
        val newPointerLeft = "├──"
        newBuildUp + loop(left, buildUp, newPadding, newPointerLeft) + loop(right, buildUp, newPadding, newPointerRight)
      }
      case Literal(cur) => {
        val newBuildUp = buildUp + padding + pointer + cur + "\n"
        newBuildUp
      }
      case Zero => {
        val newBuildUp = buildUp + padding + pointer + "0" + "\n"
        newBuildUp
      }
      case _ => ""

    }
    loop(t, "", "", "")
  }

}