package common.data_structures

object Trees {

  trait BinaryTree[A]

  case object Leaf extends BinaryTree[Nothing]

  case class Branch[A](node: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

}
