package com.example

import scala.util._
import scala.io.StdIn.readLine
import com.example.Suit._
import com.example.Card._
import com.example.Hands._

object Main {
  type DiscardList = Seq[Card]
  type Deck = Seq[Card]


  def main(args: Array[String]){
    val deck = Random.shuffle(Card.allCards)
    val hand = getHand(deck).get
    println(hand._1)
    println(pokerHand(hand._1))
    println(getDiscardList(hand._1))
    println(hand._1)
    
//    val maybeInt = Option(3)
//    val func: Int => Int = _ * 2
//    println(maybeInt.fmap(func))
//    println(fmap(func)(maybeInt))
  }
  
  //drawHand :: Deck -> DiscardList -> Hand -> Maybe (Hand, Deck)
  def drawHand(d: Deck, dl: DiscardList, h: Hand): Option[(Hand, Deck)] = {
    val nl = h.filter { x => (dl.indexOf(x) == -1) }
    val nr = d.drop(5 - nl.length)
    Some((toHand((nl ++ d).take(5)).get,nr))
  }

  //getHand :: Deck -> Maybe (Hand, Deck)
  def getHand(deck: Deck): Option[(Hand, Deck)] = {
    for {
      hand <- toHand(deck.take(5))
    } yield (hand, deck.take(5))
  }

  //toIntList :: String -> Maybe [Int]
  def toIntList(str:String): Option[Seq[Int]] = {
    def reads(str:String): Seq[Int] = {
      str.map(_.asDigit)
    }
    if(str.forall(_.isDigit)) {
      Some(reads(str))
    } else {
      None
    }
  }

  //selectByIndexes :: [a] -> [Int] -> Maybe [a]
  def selectByIndexes(l:Hand, ns:Seq[Int]): Option[Seq[Card]] = {
    if (ns.exists(x => (1 > x || x > 5 ))) {
      None
    } else {
      Some(ns.map(
          (x:Int) => l.apply(x.-(1))
      ))
    }     
  }
  
  //getDiscardList :: Hand -> IO (Maybe DiscardList)
  def getDiscardList(h: Hand): Option[DiscardList] = {
      val input = readLine();
    for {
      intList <- toIntList(input)
      res <- selectByIndexes(h, intList)
    } yield (res)
  }
}
