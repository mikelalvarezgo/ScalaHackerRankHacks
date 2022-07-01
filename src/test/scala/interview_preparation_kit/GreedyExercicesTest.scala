package interview_preparation_kit

import interview_preparation_kit.Greedy._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

final class GreedyExercicesTest extends AsyncWordSpec with Matchers {

  "Minimum absolute difference function" should {
    "return expected result" in {
      minimumAbsoluteDifference(Array(3, -7, 0)) shouldBe 3
    }
    "Luck balance function" should {
      "return expected result" in {
        luckBalance(2, Array(Array(5, 1), Array(1, 1), Array(4, 0))) shouldBe 10
      }
    }
  }
}
