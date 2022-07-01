package interview_preparation_kit

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import interview_preparation_kit.Graphs._

final class GraphExercisesTest extends AsyncWordSpec with Matchers {

  "Library pairs function" should {
    "return expected result" in {
      roadsAndLibraries(10, 3, 2,
        Array(Array(1, 7), Array(1, 3), Array(1, 2), Array(2, 3), Array(5, 6), Array(6, 8))
      ) shouldBe 16

      roadsAndLibraries(10, 2, 2,
        Array(Array(1, 7), Array(1, 3), Array(1, 2), Array(2, 3), Array(5, 6), Array(6, 8))
      ) shouldBe 14

      roadsAndLibraries(10, 2, 1,
        Array(Array(1, 7), Array(1, 3), Array(1, 2), Array(2, 3), Array(5, 6), Array(6, 8))
      ) shouldBe 9
    }
  }

  "bfs function" should {
    "return expected result" in {
      bfs(6, 4, Array(Array(1, 2), Array(1, 3), Array(3, 4), Array(3, 5)), 1) shouldBe Array(6, 6, 12, 12, -1)

      bfs(8, 6, Array(Array(2, 3), Array(2, 4), Array(3, 5), Array(3, 6), Array(3, 7), Array(7, 8)), 1) shouldBe Array(-1, -1, -1, -1, -1, -1, -1)
      bfs(8, 6, Array(Array(2, 3), Array(2, 4), Array(3, 5), Array(3, 6), Array(3, 7), Array(7, 8)), 2) shouldBe Array(-1, 6, 6, 12, 12, 12, 18)

    }
  }
}

