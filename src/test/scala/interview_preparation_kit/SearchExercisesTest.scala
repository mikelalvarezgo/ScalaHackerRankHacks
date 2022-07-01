package interview_preparation_kit

import interview_preparation_kit.Search._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

final class SearchExercisesTest extends AsyncWordSpec with Matchers {

  "Minimum absolute difference function" should {
    "return expected result" in {
      whatFlavors(Array(1, 4, 5, 3, 2), 5) shouldBe()
    }
  }
}
