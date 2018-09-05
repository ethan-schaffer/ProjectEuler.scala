import scala.collection.mutable

object Euler62 {
  def main(args: Array[String]): Unit = {
    /*
    This scala code is for Project Euler 62: Cubic permutations

    https://projecteuler.net/problem=62

    Written by Ethan Schaffer
    */

    var foundSolution = false
    var index = 0

    var charList: List[Char] = List()
    var results: Map[List[Char], List[Long]] = Map.empty[List[Char], List[Long]]

    while(!foundSolution){
      index += 1
      val number = Math.pow(index, 3).toLong
      charList = number.toString.toList.sorted
      if(!results.contains(charList)) {
        results += (charList -> List())
      }
      val current = results(charList)

      val newList = List.concat(current, List(number))
      if(newList.length == 5){
        foundSolution = true
        println(newList)
      } else {
        results += (charList -> newList)
      }
    }


  }
}
