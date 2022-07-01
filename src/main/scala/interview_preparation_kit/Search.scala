package interview_preparation_kit

import common.FoldSyntax._

import scala.util.control.Breaks.{break, breakable}

object Search {


  // FUNCTIONAL WAY: It doesn't works in terms of efficiency in Hackerrank
  def whatFlavorsFP(cost: Array[Int], money: Int) {
    final case class State(i: Int = 0, map: Map[Int, Int] = Map.empty, break: Boolean = false) {
    }
    cost.toList.foldWhile(State())(state => !state.break) { (state, cost) =>
      val target = money - cost
      if (state.map.get(target).isDefined) {
        println(state.map.getOrElse(target, 0), cost)
        state.copy(break = true)
      } else state.copy(map = state.map + (cost -> (state.i + 1)), i = state.i + 1)
    }
  }

  //dirty way
  def whatFlavors(cost: Array[Int], money: Int): Unit = {
    var hashMap = scala.collection.mutable.HashMap.empty[Int, Int]
    breakable {
      for (a <- 0 to cost.size - 1) {
        val target = money - cost(a)
        if (hashMap.contains(target)) {
          println(s"${hashMap.getOrElse(target, 0)} ${a + 1}")
          break()
        } else hashMap.addOne(cost(a) -> (a + 1))
      }
    }
  }

  // Binary tree

  //It can be empty (null).
  //It contains a root node only.
  //It contains a root node with a left subtree, a right subtree, or both. These subtrees are also binary trees.
  trait BinaryTree[A]

  case object Leaf extends BinaryTree[Nothing]

  case class Branch[A](node: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]


}
