package structures

trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](cur: String, left: Tree[A], right: Tree[A]) extends Tree[A]
case object EmptyTree extends Tree[Nothing]

object Tree {

  def getRight[A](branch: Tree[A]): Tree[A] = branch match {
    case EmptyTree => EmptyTree
    case Branch(_, _, r) => r
    case Leaf(_) => EmptyTree
  }

  def getLeft[A](branch: Tree[A]): Tree[A] = branch match {
    case EmptyTree => EmptyTree
    case Branch(_, l, _) => l
    case Leaf(_) => EmptyTree
  }


  def pPrint[A](t: Tree[A]): String = {

    def loop(t: Tree[A], buildUp: String, padding: String, pointer: String): String = t match {
      case Branch(cur, left, right) => {
        println(cur)
        val newBuildUp = buildUp + padding + pointer + cur + "\n"
        val newPadding = padding + "│  "
        val newPointerRight = "└──"
        val newPointerLeft = "├──"

        newBuildUp + loop(left, buildUp, newPadding, newPointerLeft) + loop(right, buildUp, newPadding, newPointerRight)
      }
      case Branch(cur, left, EmptyTree) => {
        println(s"$left run?")

        val newBuildUp = buildUp + padding + pointer + cur + "\n"
        val newPadding = padding + "│  "
        val newPointer = "├──"

        loop(left, newBuildUp, newPadding, newPointer)
      }
      case Branch(cur, EmptyTree, right) => {
        println(s" $right right run")

        val newBuildUp = buildUp + padding + pointer + cur + "\n"
        val newPadding = padding + "│  "
        val newPointer = "└──"

        loop(right, newBuildUp, newPadding, newPointer)
      }

      case Leaf(cur) => {
        val newBuildUp = buildUp + padding + pointer + cur + "\n"
//        println()
       //println(s"${padding+pointer} HELLO \n")
        newBuildUp
      }
    }
    loop(t, "", "", "")
  }


}

/*
    def loop(t: Tree[A], buildUp: String, padding: String, pointer: String): String = t match {
      case EmptyTree => ""
      case Branch(cur, l, r) => {
        var pointerForLeft = ""
        val newBuild = buildUp + padding + pointer + cur + "\n" + "│  "
        if(getRight(t)!=EmptyTree)
          pointerForLeft = "├──"
        else
          pointerForLeft = "└──"
        val pointerForRight = "└──"

        pPrint(l, newBuild, padding + "│  ", pointerForLeft)
        pPrint(r, newBuild, padding + "│  ", pointerForRight)
      }
      case Leaf(cur) => println(cur)
    }

 */