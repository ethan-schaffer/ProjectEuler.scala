class Card (suit: Suit, value: Value) {
  override def toString: String = {
    value.toString.substring(0, value.toString.length-2) +
      " of " + suit.toString.substring(0, suit.toString.length-2)
  }

  def getSuit: Suit = suit
  def getValue: Value = value

}

abstract class Suit
case class Hearts() extends Suit
case class Diamonds() extends Suit
case class Spades() extends Suit
case class Clubs() extends Suit

abstract class Value
case class Ace() extends Value
case class King() extends Value
case class Queen() extends Value
case class Jack() extends Value
case class Ten() extends Value
case class Nine() extends Value
case class Eight() extends Value
case class Seven() extends Value
case class Six() extends Value
case class Five() extends Value
case class Four() extends Value
case class Three() extends Value
case class Two() extends Value