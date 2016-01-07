package com.example

import scala.util._
import com.example.Suit._
import com.example.Card._
import com.example.Hands._

object Main {

  def main(args: Array[String]){
    val randomHand = Random.shuffle(Card.allCards).take(5).toList.sortWith{
      (o1, o2) => (o1.suit.code < o2.suit.code) && (o1.n < o2.n)
    }
    println(randomHand)
    println(pokerHand(randomHand))
  }

}
