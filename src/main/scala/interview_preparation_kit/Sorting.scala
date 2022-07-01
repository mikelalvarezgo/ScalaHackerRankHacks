package interview_preparation_kit

import scala.annotation.tailrec

object Sorting {

  /*
  for (int i = 0; i < n; i++) {

    for (int j = 0; j < n - 1; j++) {
        // Swap adjacent elements if they are in decreasing order
        if (a[j] > a[j + 1]) {
            swap(a[j], a[j + 1]);
        }
    }

}
   */
  def countSwaps(a: Array[Int]) {
    def bubblesort(source: List[Int]): List[Int] = {
      @tailrec
      def sort(iteration: List[Int], source: List[Int], result: List[Int], swaps: Int): List[Int] = source match {
        case h1 :: h2 :: rest => if (h1 > h2) sort(iteration, h1 :: rest, result :+ h2, swaps + 1) else sort(iteration, h2 :: rest, result :+ h1, swaps)
        case l :: Nil => sort(iteration, Nil, result :+ l, swaps)
        case Nil => if (iteration.isEmpty) {
          println(result)
          result
        } else sort(iteration.dropRight(1), result, Nil, swaps)
      }

      sort(source, source, Nil, 0)
    }


  }

}
