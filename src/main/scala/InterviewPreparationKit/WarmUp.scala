package InterviewPreparationKit

import scala.annotation.tailrec

object WarmUp extends App {

  def sockMerchant(n: Int, ar: Array[Int]): Int = {
    val sortedArray = ar.sortWith(_ < _)
    final case class Acc(previous: Int, count: Int, result: Int) {
      def increment(x: Int): Acc = copy(x, count + 1, result)
      def nextNumber(x: Int): Acc = copy(x, 1, result + (count / 2))
      def getResult = result + (count /2)
    }
    sortedArray.toList
      .foldLeft(Acc(0, 1, 0)) { (acc, x) =>
        if (acc.previous == x) acc.increment(x) else acc.nextNumber(x)
      }
      .getResult

  }

  def countingValleys(steps: Int, path: String): Int = {
    // Write your code here
    final case class Acc(result: Int, count: Int) {
      def down: Acc = copy(result, count - 1)
      def up: Acc = copy(result, count + 1)
      def checkLevel: Acc = {
        if (count == 0) {
          copy(result + 1, count)
        }else this
      }
    }
    path
      .foldLeft(Acc(0, 0)) { (acc, x) =>
        x match {
          case 'U' => acc.up.checkLevel
          case 'D'=> acc.down
        }
      }
      .result

  }

  def jumpingOnClouds(c: Array[Int]): Int = {
    // Write your code here
    final case class Acc(jumps: Int, path: Array[Int]){
      def jump(n: Int): Acc = copy(jumps + 1, path.drop(n))
    }

    @tailrec
    def step(acc: Acc): Int = {
      acc.path.length match {
        case 0 => acc.jumps
        case 1  => acc.jumps
        case 2 if (acc.path.last == 1) => acc.jumps
        case 2 => acc.jumps + 1
        case _ =>
          val possibleJump = acc.path.tail.take(2)
          if (possibleJump.last == 1) step(acc.jump(1)) else step(acc.jump(2))
      }
    }

    step(Acc(0, c))
  }

  def repeatedString(s: String, n: Long): Long = {
    val length = s.length
    val numberOfAs = s.count(_ == 'a')
    val numberOfSubs = n / length
    val rest = n % length
    val restOfAs = s.take(rest.toInt).count(_ == 'a')
    (numberOfSubs * numberOfAs) + restOfAs
  }
}
