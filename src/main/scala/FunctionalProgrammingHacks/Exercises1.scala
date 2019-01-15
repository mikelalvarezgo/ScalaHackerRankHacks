package FunctionalProgrammingHacks

/**
  * Created by mikelalvarezgo on 15/01/2019.
  */
object Exercises1 {


  /*
  StringReduces ----> https://www.hackerrank.com/challenges/string-reductions/problem
   */

  def stringReduces(string: String): String =
    string.foldLeft("")((a: String, b: Char) => if (a.contains(b)) a else (a + b))

}
