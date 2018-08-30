import scala.collection.mutable.ListBuffer

object Euler41 {
  def main(args: Array[String]): Unit = {
    /*
    This scala code is for Project Euler 41: Pandigital prime

    https://projecteuler.net/problem=41

    Written by Ethan Schaffer
     */

    println(getSolution())
  }

  def getSolution(): Int = {
    val max = 9
    val valids: ListBuffer[Int] = new ListBuffer[Int]
    for(i <- 0 to max){
      val potential = max - i
      val list = 1 to potential
      for(permutation <- list.permutations.toList.map(v => v.toList)){
        val number = Integer.parseInt(permutation.mkString)
        if(isPrime(number)){
          valids += number
        }
      }
      if(valids.toList.nonEmpty){
        return valids.toList.max
      }
    }
    0
  }

  def isPrime(n: Int) = !Range(2, n - 1).exists(n % _ == 0)

}
