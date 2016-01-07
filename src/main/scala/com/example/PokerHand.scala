package com.example

object PokerHand {
  /*
   * HighCards --ハイ・カード（いわゆるブタ）
  | OnePair --ワンペア
  | TwoPair --ツーペア
  | ThreeOfAKind --スリーカード
  | Straight --ストレート
  | Flush --フラッシュ
  | FullHouse --フルハウス
  | FourOfAKind --フォーカード
  | StraightFlush --ストレート・フラッシュ
  */
  case object HighCards extends PokerHand(0) 
  case object OnePair extends PokerHand(1) 
  case object TwoPair extends PokerHand(2) 
  case object ThreeOfAKind extends PokerHand(3) 
  case object Straight extends PokerHand(4) 
  case object Flush extends PokerHand(5) 
  case object FullHouse extends PokerHand(6) 
  case object FourOfAKind extends PokerHand(7) 
  case object StraightFlush extends PokerHand(8) 

  val values = Array(HighCards,OnePair,TwoPair,ThreeOfAKind,Straight,Flush,FullHouse,FourOfAKind,StraightFlush)

}
sealed abstract class PokerHand(val code:Int)