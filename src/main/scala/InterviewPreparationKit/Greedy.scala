package InterviewPreparationKit

object Greedy {

    def minimumAbsoluteDifference(arr: Array[Int]): Int = {
      val sortedArr = arr.sorted(Ordering[Int])
      final case class State(last:Int, best:Int)
      println(s"sorted ${sortedArr.mkString("Array(", ", ", ")")}")
      val initialState = State(last = sortedArr.headOption.getOrElse(0), Int.MaxValue)
      println(s"initial state : $initialState")
      sortedArr.array.tail.foldLeft(initialState){
        (state, i) =>
          println(s"state $state")
          val difference = Math.abs(state.last - i)
          if(difference < state.best) state.copy(i, difference)
          else state.copy(i)
      }.best
    }

  def luckBalance(k: Int, contests: Array[Array[Int]]): Int = {
    // Write your code here
    val unimportantContests = contests.filter(_.last == 0)
    val luckFromUnimportant = unimportantContests.foldLeft(0){(x, acc) =>
      x + acc.head
    }
    val importantContestsSortered = contests.filter(_.last == 1).sortWith((a,b) => a.head > b.head)
    val maxOfContest = Math.min(k, importantContestsSortered.size)
    val luckWonFromImportant = importantContestsSortered.take(maxOfContest).foldLeft(0){(acc,x) => acc + x.head }
    val luckLostFromUnimportant = importantContestsSortered.drop(maxOfContest).foldLeft(0){(acc,x) => acc + x.head }
    luckWonFromImportant + luckFromUnimportant - luckLostFromUnimportant
  }

  // Complete the getMinimumCost function below.
  def getMinimumCost(k: Int, c: Array[Int]): Int = {
    val sortedFlower = c.sortWith((a,b) => a > b)
    case class State(index: Int, sum: Int){
      def step(flowers: Array[Int]): State =
        copy(
          index = index +1 ,
          sum = (flowers.sum * index) + sum
        )
    }
    val result =
      sortedFlower.sliding(k, k).foldLeft(State(1,0)){(state, flowers) => state.step(flowers)}

    result.sum

  }
}
