package example

import org.scalatest._

class HelloSpec extends WordSpec with Matchers {
  "Day 1 - first part" should {

    "captcha 1122 => 3" in {
      Day1.captcha("1122") should be (3)
    }

    "captcha 1111 => 4" in {
      Day1.captcha("1111") should be (4)
    }

    "captcha 1234 => 0" in {
      Day1.captcha("1234") should be (0)
    }

    "captcha 91212129 => 0" in {
      Day1.captcha("91212129") should be (9)
    }
  }

  "Day 1 - first part" should {
    "captcha 1212 => 6" in {
      Day1.captcha2("1212") should be (6)
    }

    "captcha 1221 => 0" in {
      Day1.captcha2("1221") should be (0)
    }

    "captcha 123425 => 4" in {
      Day1.captcha2("123425") should be (4)
    }

    "captcha 123123 => 12" in {
      Day1.captcha2("123123") should be (12)
    }

    "captcha 12131415 => 4" in {
      Day1.captcha2("12131415") should be (4)
    }
  }

  "Day 2" should {
    "Parse input" in {
      val input = "5\t1\t9\t5\n7\t5\t3\n2\t4\t6\t8"

      Day2.parseInput(input) should be (List(
        List(5,1,9,5),
        List(7, 5, 3),
        List(2,4,6,8)
      ))
    }

    "Checksum 1 => 18" in {
      val input = List(
        List(5,1,9,5),
        List(7, 5, 3),
        List(2,4,6,8)
      )

      Day2.checksum(input) should be (18)
    }

    "Checksum 2 => 9" in {
      val input = List(
        List(5,9,2,8),
        List(9,4,7,3),
        List(3,8,6,5)
      )

      Day2.checksum2(input) should be (9)
    }

  }
}
