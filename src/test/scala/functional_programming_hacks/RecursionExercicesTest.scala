package FunctionalProgrammingHacks

import FunctionalProgrammingHacks.RecursionExercises._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

/**
 * Created by mikelalvarezgo on 15/01/2019.
 */
final class RecursionExercicesTest extends AsyncWordSpec with Matchers {

  "String reduces function" should {
    "return expected result" in {

      stringReduces("aaaaaabbbbbbbccccc") shouldBe "abc"

      stringReduces("aerererere") shouldBe "aer"

      stringReduces("  ") shouldBe " "

      stringReduces("Hello World!") shouldBe "Helo Wrd!"
    }
  }
  "String compression function" should {
    "return expected result" in {

      stringCompression("aaabccdd") shouldBe "abcd"

      stringCompression("faceeface") shouldBe "faceface"

      stringCompression("") shouldBe ""

      stringCompression("a") shouldBe "a"
    }
  }

  "Filtered members repeated less than K times" should {
    "return expected result for any type" in {

      val list1 = List(1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5)
      filterOnly[Int](list1, 3) shouldBe List(1, 3, 5)

      val list2 = List('a', 'a', 'a', 'b', 'b', 'c', 'c', 'c')
      filterOnly[Char](list2, 3) shouldBe List('a', 'c')
    }
  }

  "Super digits" should {
    "return the expected results" in {

      superDigit(148, 3) shouldBe 3

    }
  }

}
