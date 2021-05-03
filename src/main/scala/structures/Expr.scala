package structures

trait Expr
case class BinOp(op: String, arg1: Expr, arg2: Expr) extends Expr
case class Literal(value: String) extends Expr
case object zero extends Expr


object Expr {
  def pPrint[A](t: Expr): String = {

    def loop(t: Expr, buildUp: String, padding: String, pointer: String): String = t match {
      case BinOp(cur, left, zero) => {
        val newBuildUp = buildUp + padding + pointer + cur + "\n"
        val newPadding = padding + "│  "
        val newPointer = "├──"
        loop(left, newBuildUp, newPadding, newPointer)
      }
      case BinOp(cur, zero, right) => {
        val newBuildUp = buildUp + padding + pointer + cur + "\n"
        val newPadding = padding + "│  "
        val newPointer = "└──"

        loop(right, newBuildUp, newPadding, newPointer)
      }
      case BinOp(cur, left, right) => {
        println(cur)
        val newBuildUp = buildUp + padding + pointer + cur + "\n"
        val newPadding = padding + "│  "
        val newPointerRight = "└──"
        val newPointerLeft = "├──"

        println(s"$left and $right")

        newBuildUp + loop(left, buildUp, newPadding, newPointerLeft) + loop(right, buildUp, newPadding, newPointerRight)
      }
      case Literal(cur) => {
        val newBuildUp = buildUp + padding + pointer + cur + "\n"
        newBuildUp
      }
      case a => {
        println(t + " hello" + a)
        ""
      }
    }
    loop(t, "", "", "")
  }

}