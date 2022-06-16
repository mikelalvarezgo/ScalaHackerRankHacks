package FunctionalProgrammingHacks

import scala.annotation.tailrec

/**
  * Created by mikelalvarezgo on 15/01/2019.
  */
object RecursionExercises {

  /*
  StringReduces ----> https://www.hackerrank.com/challenges/string-reductions/problem
   */

  def stringReduces(string: String): String =
    string.foldLeft("")((a: String, b: Char) => if (a.contains(b)) a else (a + b))

  /*
  String comprenssion -----> https://www.hackerrank.com/challenges/string-compression/problem
   */

  def stringCompression(string: String): String = {
    @tailrec
    def recCompression(count: Int, tail: String, accString: String): String = tail.length match {
      case 0 => accString
      case 1 => accString + tail.head
      case _ =>
        if (tail.head == tail.tail.head)
          recCompression(count + 1, tail.tail, accString)
        else {
          val newAccString = if (count != 1) s"$accString${tail.head}" else s"$accString${tail.head}"
          recCompression(1, tail.tail, newAccString)
        }
    }
    recCompression(1, string, "")
  }

  /*
  Filter elements : https://www.hackerrank.com/challenges/filter-elements/problem

  Given a list of N integers A = [a1, a2, ..., aN], you have to find those integers which are repeated at least K times. In case no such element exists you have to print -1.

  If there are multiple elements in A which are repeated at least K times, then print these elements ordered by their first occurrence in the list.
   */

  def filterOnly[A](l: List[A], k: Int) = {
    @tailrec
    def recFilterOnly(l: List[A], k: Int, filtered: List[A]): List[A] = l match {
      case Nil         => filtered
      case head :: Nil => if (k == 1 && !filtered.contains(head)) filtered :+ head else filtered
      case head :: tail =>
        if ((tail.filter(_ == head).size >= k - 1) && (!filtered.contains(head)))
          recFilterOnly(tail, k, filtered :+ head)
        else recFilterOnly(tail, k, filtered)

    }
    recFilterOnly(l, k, Nil)

  }

  /*
  https://www.hackerrank.com/challenges/super-digit/problem

   */

  def superDigit(l: Int, k: Int) = {
    val extendedInt = (1 to k).toList.foldLeft("")((a, b) => a ++ l.toString)

    def recSuperDigitCalc(i: Int): Int = i match {
      case d: Int if (d < 10) => d
      case d: Int =>
        recSuperDigitCalc(d.toString.foldLeft { 0 } { (a, b) =>
          a + (b.toString.toInt)
        })

    }
    recSuperDigitCalc(extendedInt.toInt)
  }

}
