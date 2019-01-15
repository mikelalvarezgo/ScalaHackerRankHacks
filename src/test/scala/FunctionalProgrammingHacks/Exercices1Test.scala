package FunctionalProgrammingHacks
package test
import org.scalatest.{FlatSpec, Matchers}
import Exercises1._

/**
  * Created by mikelalvarezgo on 15/01/2019.
  */
class Exercices1Test extends FlatSpec with Matchers {

  "String reduces functions" should  "return expected result" in {

    stringReduces("aaaaaabbbbbbbccccc") shouldBe   "abc"

    stringReduces("aerererere") shouldBe "aer"

    stringReduces("  ") shouldBe " "

    stringReduces("Hello World!") shouldBe "Helo Wrd!"
   }

}
