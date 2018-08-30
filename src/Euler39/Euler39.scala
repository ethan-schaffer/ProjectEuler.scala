import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Euler39 {
  def main(args: Array[String]): Unit = {
    /*
    This scala code is for Project Euler 39: Integer right triangles

    https://projecteuler.net/problem=39
    
    Written by Ethan Schaffer
     */

    val largestNumber = 1000
    var list: ListBuffer[Int] = new ListBuffer[Int]
    for (i <- 1 to largestNumber) {
      for (j <- i to largestNumber) {
        val hypotenuse = getThirdSide(i, j)
        if (hypotenuse != 0) {
          if (i + j + hypotenuse < largestNumber) {
            list += i + j + hypotenuse
          }
        }
      }
    }
    val grouped = list.toList.groupBy(v => v)
    val sorted = grouped.toList.sortBy(v => v._2.length).reverse
    val solution = sorted.head._1
    println(solution)

  }

  def getThirdSide(legA: Int, legB: Int): Int = {
    val sqrt = Math.sqrt(legA * legA + legB * legB)
    if (isInteger(sqrt)) {
      sqrt.toInt
    } else {
      0
    }
  }

  def isInteger(number: Double): Boolean = {
    Math.ceil(number) == Math.floor(number);
  }

}