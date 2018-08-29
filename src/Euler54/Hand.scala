class Hand(cards: Set[Card]) {

  override def toString: String = {
    val s: StringBuilder = new StringBuilder
    for (card <- cards) {
      s ++= card.toString + ", "
    }
    s.mkString.substring(0, s.mkString.length - 2)
  }

  def getNumericValue(value: Value): Int = {
    value match {
      case Ace() => 14
      case King() => 13
      case Queen() => 12
      case Jack() => 11
      case Ten() => 10
      case Nine() => 9
      case Eight() => 8
      case Seven() => 7
      case Six() => 6
      case Five() => 5
      case Four() => 4
      case Three() => 3
      case Two() => 2
    }
  }

  def getHandValue: HandValue = {
    val cardsList = cards.toList

    //Check for Pairs
    val groups = cardsList.groupBy(_.getValue)
    if (groups.size != 5) {
      //There is a pair, as we have two values that are the same
      val sizes = groups.map(c => (getNumericValue(c._1), c._2.length)).toList.sortBy(v => v._2).reverse
      sizes match {
        case List((four, 4), (one, 1)) => return FourOfAKind(four, one)
        case List((three, 3), (two, 2)) => return FullHouse(three, two)
        case List((three, 3), (one, 1), (secondOne, 1)) => if (one > secondOne) {
          return ThreeOfAKind(three, one, secondOne)
        } else {
          return ThreeOfAKind(three, secondOne, one)
        }
        case List((two, 2), (secondTwo, 2), (one, 1)) => if (two > secondTwo) {
          return TwoPair(two, secondTwo, one)
        } else {
          return TwoPair(secondTwo, two, one)
        }
        case List((two, 2), (one, 1), (secondOne, 1), (thirdOne, 1)) =>
          val lst = List(one, secondOne, thirdOne).sorted.reverse
          return Pair(two, lst.head, lst(1), lst.last)
      }
    }


    //Check for Flush
    val isFlush = cardsList.map(c => c.getSuit).distinct.size == 1

    val values = groups.keys.toList.map(getNumericValue).sorted.reverse
    var lastValue = 0
    var isStraight = true
    for (value <- values) {
      if (lastValue != 0 && lastValue - 1 != value) {
        isStraight = false
      }
      lastValue = value
    }

    if(isStraight && isFlush){
      return StraightFlush(values.head)
    }
    if(isStraight){
      return Straight(values.head)
    }

    if(isFlush){
      return Flush(values.head, values(2), values(3), values(4), values.last)
    }
    Nothing(values.head, values(1), values(2), values(3), values.last)
  }

}

abstract class HandValue {
  def beats(handValue: HandValue): Boolean
}

case class StraightFlush(val1: Int) extends HandValue {
  override def beats(handValue: HandValue): Boolean = {
    handValue match {
      case StraightFlush(v1) => val1 > v1
      case _ => true
    }
  }
}

case class FourOfAKind(val1: Int, val2: Int) extends HandValue {
  override def beats(handValue: HandValue): Boolean = {
    handValue match {
      case StraightFlush(_) => false
      case FourOfAKind(v1, v2) => val1 * 1000 + val2 > v1 * 1000 + v2
      case _ => true
    }
  }
}

case class FullHouse(val1: Int, val2: Int) extends HandValue {
  override def beats(handValue: HandValue): Boolean = {
    handValue match {
      case StraightFlush(_) => false
      case FourOfAKind(_, _) => false
      case FullHouse(v1, v2) => val1 * 1000 + val2 > v1 * 1000 + v2
      case _ => true
    }
  }
}

case class Flush(val1: Int, val2: Int, val3: Int, val4: Int, val5: Int) extends HandValue {
  override def beats(handValue: HandValue): Boolean = {
    handValue match {
      case StraightFlush(_) => false
      case FourOfAKind(_, _) => false
      case FullHouse(_, _) => false
      case Flush(v1, v2, v3, v4, v5) =>
        val hand = List(val1, val2, val3, val4, val5)
        val other = List(v1, v2, v3, v4, v5)
        for (v <- hand.indices) {
          if (hand(v) > other(v)) {
            return true
          }
          if (hand(v) < other(v)) {
            return false
          }
        }
        return false
      case _ => true
    }
  }
}

case class Straight(val1: Int) extends HandValue {
  override def beats(handValue: HandValue): Boolean = {
    handValue match {
      case StraightFlush(_) => false
      case FourOfAKind(_, _) => false
      case FullHouse(_, _) => false
      case Straight(v1) => val1 > v1
      case _ => true
    }
  }
}

case class ThreeOfAKind(val1: Int, val2: Int, val3: Int) extends HandValue {
  override def beats(handValue: HandValue): Boolean = {
    handValue match {
      case Nothing(_, _, _, _, _) => true
      case Pair(_, _, _, _) => true
      case TwoPair(_, _, _) => true
      case ThreeOfAKind(v1, v2, v3) =>
        val hand = List(val1, val2, val3)
        val other = List(v1, v2, v3)
        for (v <- hand.indices) {
          if (hand(v) > other(v)) {
            return true
          }
          if (hand(v) < other(v)) {
            return false
          }
        }
        false
      case _ => false
    }
  }
}

case class TwoPair(val1: Int, val2: Int, val3: Int) extends HandValue {
  override def beats(handValue: HandValue): Boolean = {
    handValue match {
      case Nothing(_, _, _, _, _) => true
      case Pair(_, _, _, _) => true
      case TwoPair(v1, v2, v3) =>
        val hand = List(val1, val2, val3)
        val other = List(v1, v2, v3)
        for (v <- hand.indices) {
          if (hand(v) > other(v)) {
            return true
          }
          if (hand(v) < other(v)) {
            return false
          }
        }
        false
      case _ => false
    }
  }
}

case class Pair(val1: Int, val2: Int, val3: Int, val4: Int) extends HandValue {
  override def beats(handValue: HandValue): Boolean = {
    handValue match {
      case Nothing(_, _, _, _, _) => true
      case Pair(v1, v2, v3, v4) =>
        val hand = List(val1, val2, val3, val4)
        val other = List(v1, v2, v3, v4)
        for (v <- hand.indices) {
          if (hand(v) > other(v)) {
            return true
          }
          if (hand(v) < other(v)) {
            return false
          }
        }
        return false
      case _ => false
    }
  }
}

case class Nothing(val1: Int, val2: Int, val3: Int, val4: Int, val5: Int) extends HandValue {
  override def beats(handValue: HandValue): Boolean = {
    handValue match {
      case Nothing(v1, v2, v3, v4, v5) =>
        val hand = List(val1, val2, val3, val4, val5)
        val other = List(v1, v2, v3, v4, v5)
        for (v <- hand.indices) {
          if (hand(v) > other(v)) {
            return true
          }
          if (hand(v) < other(v)) {
            return false
          }
        }
        return false
      case _ => false
    }
  }
}
