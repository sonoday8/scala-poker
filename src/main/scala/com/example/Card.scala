package com.example

import scala.language.implicitConversions._

import com.example.Suit._

object Card {

  def showCardNumber(n: Int): String = {
    n match {
      case 14 => "A_"
      case 13 => "K_"
      case 12 => "Q_"
      case 11 => "J_"
      case 10 => "10"
      case _  => n.toString() + "_"
    }
  }
  
  def allCards: Seq[Card] = {
    for {
      s <- Suit.values.toSeq
      i <- 2 to 14
    }  yield Card(i, s)
  }


}

//sealed abstract class Card(n: Int, suit: Suit)
final case class Card(n: Int, suit: Suit){
  override def toString: String = {
    suit match {
      case Hearts => "H" + Card.showCardNumber(n)
      case Diamonds => "D" + Card.showCardNumber(n)
      case Clubs => "C" + Card.showCardNumber(n)
      case Spades => "S" + Card.showCardNumber(n)
    }
  }
}
