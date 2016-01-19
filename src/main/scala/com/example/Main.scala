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
  }
  
  //

  /**
   * drawHand deck _discards _hand = let
  nl = filter (flip notElem _discards) (fromHand _hand)
  nr = drop (5 - length nl) deck
  in  (,) <$> toHand (take 5 $ nl ++ deck) <*> Just nr
   * 
   */
  //drawHand :: Deck -> DiscardList -> Hand -> Maybe (Hand, Deck)
  def drawHand(d: Deck, dl: DiscardList, h: Hand): Option[(Hand, Deck)] = {
    val nl = h.filter { x => (dl.indexOf(x) == -1) }
    val nr = d.drop(5 - nl.length)
    Some((toHand((nl ++ d).take(5)).get,nr))
  }

  //getHand :: Deck -> Maybe (Hand, Deck)
  /**
  getHand deck = do
    hand <- toHand . take 5 $ deck
    return (hand, drop 5 deck)
   */
  def getHand(deck: Deck): Option[(Hand, Deck)] = {
    for {
      hand <- toHand(deck.take(5))
    } yield (hand, deck.take(5))
  }

  /*
toIntList :: String -> Maybe [Int]
toIntList str = if and $ map isDigit str then Just $ reads str else Nothing
  where
    reads :: String -> [Int]
    reads = map $ read . (:[])
   */
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
/*
selectByIndexes :: [a] -> [Int] -> Maybe [a]
selectByIndexes l = map ((l!!).(subtract 1))
selectByIndexes l = sequence . map ((atMay l).(subtract 1))
*/
  def selectByIndexes[A](l:Seq[A], ns:Seq[Int]): Option[Seq[A]] = {
    Some(ns.map((x:Int) => l.apply(x.-(1))))
  }
  

/*getDiscardList :: Hand -> IO (Maybe DiscardList)
getDiscardList h = do
    input <- getLine
    return $ do
         intList <- toIntList input
         res <- selectByIndexes (fromHand h) intList
         return res

*/  
  //getDiscardList :: Hand -> IO (Maybe DiscardList)
  def getDiscardList(h: Hand): Option[DiscardList] = {
      val input = readLine();
    for {
      intList <- toIntList(input)
      res <- selectByIndexes(h, intList)
    } yield (res)
    //None
  }
}
