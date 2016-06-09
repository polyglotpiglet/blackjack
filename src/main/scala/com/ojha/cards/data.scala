package com.ojha.cards

import cats.Show

/**
  * Created by alexbate on 09/06/16.
  */
object data {

  trait Suit

  object Suits {
    case object Spade extends Suit
    case object Club extends Suit
    case object Diamond extends Suit
    case object Heart extends Suit

    val ALL: List[Suit] = Spade :: Club :: Diamond :: Heart :: Nil
  }

  trait Rank {
    def score: Int
  }

  object Ranks {
    case object Ace extends Rank {
      override def score: Int = 10
    }
    case object Two extends Rank {
      override def score: Int = 2
    }
    case object Three extends Rank {
      override def score: Int = 3
    }
    // ... etc etc

    val All: List[Rank] = Ace :: Two :: Three :: Nil
  }

  case class Card(suit: Suit, rank: Rank)

  implicit object ShowCard extends Show[Card] {
    override def show(hand: Card): String = s"${hand.rank} of ${hand.suit}s"
  }

  case class Deck(cards: Seq[Card])



  //object Ranks {
  //  case object Ace extends Rank
  //  case object Two extends Rank
  //  case object Three extends Rank
  //  case object Four extends Rank
  //  case object Five extends Rank
  //  case object Six extends Rank
  //  case object Seven extends Rank
  //  case object Eight extends Rank
  //  case object Nine extends Rank
  //  case object Ten extends Rank
  //  case object Jack extends Rank
  //  case object Queen extends Rank
  //  case object King extends Rank
  //}
}


