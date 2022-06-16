package InterviewPreparationKit

object WarmUp extends App{

  def sockMerchant(n: Int, ar: Array[Int]): Int = {
    val sortedArray = ar.sortWith(_ < _)
    println(sortedArray)
    final case class Acc(previous: Int, count: Int, result: Int){
      def increment(x: Int): Acc = copy(x, count + 1, result)
      def nextNumber(x:Int): Acc = copy(x, 1, result + (count / 2))
    }
    sortedArray
      .toList.
      foldLeft(Acc(0,1,0)) {
        (acc, x) => if(acc.previous == x) acc.increment(x) else acc.nextNumber(x)
      }.result

  }

  val result = sockMerchant(10, Array(1, 1, 3, 1, 2, 1, 3, 3, 3, 3))
  println(result)
}
