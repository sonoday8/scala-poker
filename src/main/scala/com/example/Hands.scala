package com.example

import com.example.PokerHand._

object Hands {
  def groupBy[A](f: (A, A) => Boolean, lst:List[A]): List[List[A]] ={
    def span[A](f: (A) => Boolean, lst:List[A]): (List[A],List[A]) = {
      lst match {
        case (x :: xs) => {
          if (f(x)) {
            val (ys,zs) = span(f,xs)
            (x::ys,zs)
          } else {
            (Nil,lst)
          }
        }
        case _ => (Nil,lst)
      }
    }
    lst match {
      case (x :: xs) => {
        //val fcurried = f.curried
        val (ys,zs) = span(f.curried(x),xs)
        (x::ys)::(groupBy(f,zs))
      }
      case _ => List(Nil)
    }
  }

  def mplus[A](a:Option[A], b:Option[A]): Option[A] = { 
    a match {
      case Some(x) => Some(x)
      case _ => b
    }
  }

  def fst[T1, T2](x:(T1,T2)):T1 = {
    x._1
  }
  
  def snd[T1, T2](x:(T1,T2)):T2 = {
    x._2
  }
  //extract :: (b -> a) -> [b] -> [(a, b)]
  def extract[A,B](f:(B)=>A, l:List[B]): List[(A,B)] = {
    l.map(c => (f(c),c))
  }
  
  type Hand = List[Card]
  
  //pokerHand :: Hand -> (PokerHand, Card)
  def pokerHand(h:Hand):(PokerHand, Card) = {
    val hands = List(straightFlush(h)
        , fourOfAKind(h)
      , fullHouse(h)
      , flush(h)
      , straight(h)
      , threeOfAKind(h)
      , twoPair(h)
      , onePair(h) )
    val hs = hands.filter{ x => 
      x match { 
        case Some(h) => true
        case _ => false
      }
    }.map(_.get)
    if (hs.length == 0) {
      (HighCards,h.maxBy(_.suit.code))
    } else {
      hs.maxBy(_._1.code)
    }
  }

  //straightFlush :: Hand -> Maybe (PokerHand, Card)
  def straightFlush(h: Hand):Option[(PokerHand, Card)]  = {
    for {
      c <- straightHint(h)
      d <- flushHint(h)
    } yield(StraightFlush, List(c, d).maxBy(_.suit.code))
  }
  
  //fourOfAKind :: Hand -> Maybe (PokerHand, Card)
  def fourOfAKind(h: Hand):Option[(PokerHand, Card)]  = {
    for {
      cs <- nOfKindHint(4,h)
    } yield(FourOfAKind, cs.flatten.maxBy(_.suit.code))
  } 


  //fullHouse :: Hand -> Maybe (PokerHand, Card)
  def fullHouse(h: Hand):Option[(PokerHand, Card)]  = {
    for {
      cs <- nOfKindHint(3,h)
      _ <- nOfKindHint(2,h)
    } yield(FullHouse, cs.flatten.maxBy(_.suit.code))
  } 

  //flush :: Hand -> Maybe (PokerHand, Card)
  def flush(h: Hand):Option[(PokerHand, Card)]  = {
    for {
      cs <- flushHint(h)
    } yield(Flush, cs)
} 

  //straight :: Hand -> Maybe (PokerHand, Card)
  def straight(h: Hand):Option[(PokerHand, Card)]  = {
    for {
      cs <- straightHint(h)
    } yield(Straight, cs)
  } 

  //threeOfAKind :: Hand -> Maybe (PokerHand, Card)
  def threeOfAKind(h: Hand):Option[(PokerHand, Card)]  = {
    for {
      cs <- nOfKindHint(3,h)
    } yield(ThreeOfAKind, cs.flatten.maxBy(_.suit.code))
  } 

  //twoPair :: Hand -> Maybe (PokerHand, Card)
  def twoPair(h: Hand):Option[(PokerHand, Card)]  = {
    for {
      cs <- nOfKindHint(2,h)
    } yield(TwoPair, cs.flatten.maxBy(_.suit.code))
  } 

  //onePair :: Hand -> Maybe (PokerHand, Card)
  def onePair(h: Hand):Option[(PokerHand, Card)]  = {
    for {
      cs <- nOfKindHint(2,h)
    } yield(OnePair,cs.flatten.maxBy(_.suit.code))
  } 

  def toHand(cards: List[Card]): Option[Hand] = {
    if (cards.length == 5) {
      Some(cards.sortWith((o1, o2) => (o1.suit.code < o2.suit.code) && (o1.n < o2.n)))
    } else {
      None
    }
  }
  
  //flushHint :: Hand -> Maybe Card
  def flushHint(h:Hand): Option[Card] = {
    h match {
      case (x::xs) => {
        if(xs.forall((c:Card)=>x.suit == c.suit)) {
          Some(xs.last)
        } else {
          None
        }
      }
      case _ => None
    }
  }

  /**
   * nOfKindHint :: Int -> Hand -> Maybe [[Card]]
  nOfKindHint n (Hand h) = if cards /= [] then Just cards else Nothing
    where
      cards :: [[Card]]
      cards = filter ((==n).length) 
        $ groupBy (\x y -> cardNumber x == cardNumber y) h
   */
  def nOfKindHint(n:Int, h:Hand): Option[List[List[Card]]] = {
    def cards(h:Hand): List[List[Card]] = {
      groupBy(((x:Card, y:Card)=> x.n == y.n),h).filter{x:List[Card] => x.length == n}
    }
    val _cards = cards(h)
    if(_cards != Nil) {
      Some(_cards)
    } else {
      None
    }
  }
  
/*  
straightHint :: Hand -> Maybe Card
straightHint (Hand l) = 
  (judgeStraight . extract cardStrength $ l)
  `mplus`
  (judgeStraight . sort . extract cardNumber $ l)
    where
      isStraight :: [Int] -> Bool
      isStraight xs@(x:_) = xs == [x .. x + 4]
      isStraight _ = False
      
      judgeStraight :: [(Int, Card)] -> Maybe Card
      judgeStraight l = 
        if isStraight $ map fst l
          then Just . snd . last $ l
          else Nothing
*/
  def straightHint(h:Hand):Option[Card] = {
    //isStraight :: [Int] -> Bool
    def isStraight(xs:List[Int]): Boolean = {
      xs match {
        case (x::_) => (xs == (x to (x+4)))
        case _ => false
      } 
    }
    //judgeStraight :: [(Int, Card)] -> Maybe Card
    def judgeStraight(l:List[(Int, Card)]): Option[Card] = {
      if (isStraight(l.map(fst))) {
        Some(snd(l.last))
      } else {
        None
      }
    }
    //cardStrength :: Card -> Int
    def cardStrength(c:Card):Int = c.n
    def cardNumber(c:Card):Int = c.n
    
    val a = (judgeStraight(extract(cardStrength,h)))
    val b =(judgeStraight(extract(cardNumber,h).sortWith((c1,c2)=>(c1._1 < c2._1)))) 
    mplus(a,b)
    
  }
  
}