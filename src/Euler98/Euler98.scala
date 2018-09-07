import scala.io.Source

object Euler98 {
  /*
  This scala code is for Project Euler 98: Anagramic squares

  https://projecteuler.net/problem=98

  Written by Ethan Schaffer
  */

  def main(args: Array[String]): Unit = {
    val filename = "src/Euler98/words.txt"
    val line = Source.fromFile(filename).getLines.toList.head
    val listOfWords = line.replaceAll("\"", "").split(",")
    var map: Map[List[Char], List[String]] = Map.empty[List[Char], List[String]]
    for (word <- listOfWords) {
      val sortedChars = word.toList.sorted
      if (!map.contains(sortedChars)) {
        map += (sortedChars -> List())
      }
      val currentWords = map(sortedChars)
      map += (sortedChars -> List.concat(currentWords, List(word)))
    }

    map = map.filter(v => v._2.length > 1)
    //println(map)

    val permutations = List(1, 2, 3, 4, 5, 6, 7, 8, 9).permutations.toList
    //println(permutations)

    var max = 0
    for ((letters, words) <- map.toList) {
      if(max < Math.pow(10, letters.length).toInt) {
        for (permute <- permutations.map(v => v.slice(0, letters.length)).distinct) {
          val potentialSquare = Integer.parseInt(permute.mkString)
          if (isSquare(potentialSquare)) {
            var letterToNumber: Map[Char, Int] = Map.empty[Char, Int]
            for (letter <- words.head) {
              letterToNumber += (letter -> permute(words.head.indexOf(letter)))
            }
            var secondarySquare = 0
            for (letter <- words.last) {
              secondarySquare *= 10
              secondarySquare += letterToNumber(letter)
            }
            if (isSquare(secondarySquare)) {
              max = List(potentialSquare, secondarySquare, max).max
            }
          }
        }
      }
    }
    println(max)

  }

  def isSquare(number: Int): Boolean = {
    isInteger(Math.sqrt(number))
  }

  def isInteger(number: Double): Boolean = {
    Math.ceil(number) == Math.floor(number)
  }

}
