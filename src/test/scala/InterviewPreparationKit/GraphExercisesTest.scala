package InterviewPreparationKit

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import InterviewPreparationKit.Graphs._

final class GraphExercisesTest extends AsyncWordSpec with Matchers {

  "Library pairs function" should {
    "return expected result" in {
      roadsAndLibraries(10, 3, 2,
        Array(Array(1,7),Array(1,3),Array(1,2),Array(2,3),Array(5,6), Array(6,8))
      ) shouldBe 16

      roadsAndLibraries(10, 2, 2,
        Array(Array(1,7),Array(1,3),Array(1,2),Array(2,3),Array(5,6), Array(6,8))
      ) shouldBe 14

      roadsAndLibraries(10, 2, 1,
        Array(Array(1,7),Array(1,3),Array(1,2),Array(2,3),Array(5,6), Array(6,8))
      ) shouldBe 9
    }
  }
}

