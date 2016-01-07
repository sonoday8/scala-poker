package com.example

object Suit {
  case object Hearts extends Suit(0) 
  case object Diamonds extends Suit(1) 
  case object Clubs extends Suit(2) 
  case object Spades extends Suit(3) 

  val values = Array(Hearts,Diamonds,Clubs,Spades)
  
//  private val codeTable = Array(Hearts,Diamonds,Clubs,Spades)
//  def complement(code:Int) : Suit = codeTable((~code & 0x03) | (code & 0x04))
}

// sealedを付けると、Suitを拡張したクラスはこのファイル内でしか定義できない
// abstractを付けると、Suitを拡張したクラスはHearts,Diamonds,Clubs,Spades 以外にないことを保証できるので
// match文がexhaustive(すべてのケースを網羅)になる
sealed abstract class Suit(val code:Int) {
//    val id = code    
//    val name = toString
//    def complement = Suit.complement(code)
}
