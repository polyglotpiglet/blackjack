package com.ojha.cards

import cats.Show
import com.ojha.cards.data._
import cats.syntax.show._

/**
  * Created by alexbate on 09/06/16.
  */
object blackjackdata {

  abstract class BlackJackHand(theCards: Seq[Card]) {
    val cards = theCards
    def size: Int = cards.size
    def visibleCards: Seq[Card]
    def :+(card: Card): BlackJackHand

    def score: Int = {
      cards.foldLeft(0) {
        case (acc, card) => acc + card.rank.score
      }
    }
  }

  // an open hand is fully visible
  case class OpenHand(override val cards: Seq[Card]) extends BlackJackHand(cards) {
    override def visibleCards: Seq[Card] = cards
    override def :+(card: Card): BlackJackHand = new OpenHand(cards :+ card)
  }

  // in a partially open hand only one card is visible
  case class PartiallyOpenHand(override val cards: Seq[Card]) extends BlackJackHand(cards) {
    override def visibleCards: Seq[Card] = cards.head :: Nil
    override def :+(card: Card): BlackJackHand = new PartiallyOpenHand(cards :+ card)
    def toOpen: OpenHand = new OpenHand(cards)
  }

  implicit object ShowBlackJackHand extends Show[BlackJackHand] {
    override def show(hand: BlackJackHand): String = s"Hand(${hand.visibleCards.map(_.show).mkString(", ")})"
  }

  case class DealtDeck(remainingDeck: Deck, playerHand: BlackJackHand, dealerHand: BlackJackHand)

  implicit object ShowDealtDeck extends Show[DealtDeck] {
    override def show(dealtDeck: DealtDeck): String = {
      "-----------------------------------------------------------------------------\n" +
      s"Dealer cards:\t\tScore = ${dealtDeck.dealerHand.score}\t\t ${dealtDeck.dealerHand.show}\n" +
      s"Player cards:\t\tScore = ${dealtDeck.playerHand.score}\t\t ${dealtDeck.playerHand.show}\n" +
      "-----------------------------------------------------------------------------\n"
    }
  }


}

