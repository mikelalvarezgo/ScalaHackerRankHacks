package InterviewPreparationKit

import scala.annotation.tailrec

object Arrays {
  def rotLeft(a: Array[Int], d: Int): Array[Int] = {
    (1 to d).foldLeft(a) { (arr, _) => arr.tail.appended(arr.head) }
  }

  def hourglassSum(arr: Array[Array[Int]]): Int = {
    (0.to(arr.length - 3)).flatMap { row =>
      (0.to(arr.length - 3)).map { col =>
        Array(arr(row)(col), arr(row)(col + 1), arr(row)(col + 2), arr(row + 1)(col + 1), arr(row + 2)(col), arr(row + 2)(col + 1), arr(row + 2)(col + 2)).sum
      }
    }.max
  }

  def minimumBribes(q: Array[Int]) {
    val originalPosition = (1 to q.length)
    val result =
      q
        .zip(originalPosition)
        .map { case (value, index) => Math.abs(Math.abs(value) - Math.abs(index)) }
        .toSet
        .sum

    //scalastyle:off
    if (result < 3) println(result) else println("Too chaotic")

  }

  // Complete the minimumSwaps function below.
  def minimumSwaps(arr: Array[Int]): Int = {

   @tailrec
   def step(result: Array[Int], swaps: Int) = {

   }


  }

}
