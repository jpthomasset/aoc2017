package example

import scala.io.StdIn

object Hello extends App {
  Day2.run()

}

object Day1 {

  def run(): Unit = {
    println("AOC 2017 !")
    println("----------")

    print("Captcha: ")
    val input = StdIn.readLine()

    println(" => " + captcha2(input))
  }

  def captcha(input: String): Int = {
    val inputSecond = input.drop(1) + input.charAt(0)
   
    input
      .zip(inputSecond)
      .map({ case (f,s) => (f.asDigit, s.asDigit)})
      .foldLeft(0) { (a, t) => a + (if(t._1==t._2) t._1 else 0)} 
  }

  def captcha2(input: String): Int = {
    val offset = input.length() / 2
    val inputSecond = input.drop(offset) + input.take(offset)
   
    input
      .zip(inputSecond)
      .map({ case (f,s) => (f.asDigit, s.asDigit)})
      .foldLeft(0) { (a, t) => a + (if(t._1==t._2) t._1 else 0)} 
  }
}

object Day2 {

  def run(): Unit = {
    println("Day 2 !")
    println("------")

    println("Enter spreadsheet (end with empty line): ")
    val input = Iterator.
      continually(StdIn.readLine).
      takeWhile(_ != "").
      mkString("\n")

    val spreadSheet = parseInput(input)

    println(" => " + checksum2(spreadSheet))
  }

  def parseInput(input: String): List[List[Int]] = {
    input.split('\n')
    .map(line => line.split('\t').map(_.toInt).toList).toList
  }

  def checksum(input: List[List[Int]]): Int = {
    input
      .map(r => r.max - r.min)
      .foldLeft(0)( { case (acc, i) => acc + i })
  }

  def checksum2(input: List[List[Int]]): Int = {
    input
      .map(r => {
        (for {
          a <- r
          b <- r
          if a != b
          if a % b == 0
        } yield a / b).head

      })
      .foldLeft(0)( { case (acc, i) => acc + i })
  }

}
