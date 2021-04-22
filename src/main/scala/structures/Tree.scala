package structures

trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](cur: A, left: Tree[A], right: Tree[A]) extends Tree[A]
case object EmptyTree extends Tree[Nothing]

