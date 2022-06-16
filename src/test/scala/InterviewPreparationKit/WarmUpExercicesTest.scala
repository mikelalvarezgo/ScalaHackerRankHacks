package InterviewPreparationKit

import InterviewPreparationKit.WarmUp._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

final class WarmUpExercicesTest extends AsyncWordSpec with Matchers {

  "Shock pairs function" should {
    "return expected result" in {
      sockMerchant(10, Array(1, 1, 3, 1, 2, 1, 3, 3, 3, 3)) shouldBe 4
    }
  }

  "Jumping in clouds function" should {
    "return expected result" in {
      jumpingOnClouds(Array(0, 0, 1, 0, 0, 0, 0, 1, 0, 0)) shouldBe 6
    }
  }
  "Repeated string function" should{
    "return expeced result" in {
      // scalastyle:off
      repeatedString("epsxyyflvrrrxzvnoenvpegvuonodjoxfwdmcvwctmekpsnamchznsoxaklzjgrqruyzavshfbmuhdwwmpbkwcuomqhiyvuztwvq", 549382313570L)  shouldBe 16481469408L
      // scalastyle:on
    }
  }
}
