import scala.io.Source

object Euler54 {

  /*
  This scala code is for Project Euler 54: Poker hands

  https://projecteuler.net/problem=54

  Written by Ethan Schaffer
  */

  def main(args: Array[String]): Unit = {

    var playerOneWins = 0
    val filename = "src/Euler54/poker.txt"
    for (line <- Source.fromFile(filename).getLines) {
      val hand1 = new Hand(line.substring(0, 14).split(" ").map(st => getCard(st)).toSet)
      val hand2 = new Hand(line.substring(15, 29).split(" ").map(st => getCard(st)).toSet)
      if (hand1.getHandValue.beats(hand2.getHandValue)) {
        println(hand1.toString() + "  vs  " + hand2.toString())
        println(hand1.getHandValue + " beats " + hand2.getHandValue)
        println("")
        playerOneWins += 1
      }
    }
    println(playerOneWins)
  }

  def getCard(str: String): Card = {
    new Card(getSuit(str.charAt(1)), getValue(str.charAt(0)))
  }

  def getSuit(char: Char): Suit = {
    char match {
      case 'H' => Hearts()
      case 'D' => Diamonds()
      case 'C' => Clubs()
      case _ => Spades()
    }
  }

  def getValue(c: Char): Value = {
    c match {
      case 'A' => Ace()
      case 'K' => King()
      case 'Q' => Queen()
      case 'J' => Jack()
      case 'T' => Ten()
      case '9' => Nine()
      case '8' => Eight()
      case '7' => Seven()
      case '6' => Six()
      case '5' => Five()
      case '4' => Four()
      case '3' => Three()
      case '2' => Two()
    }
  }
  
}
