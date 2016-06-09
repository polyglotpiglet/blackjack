package com.ojha.cards

import data._
import cats.syntax.show._
import com.ojha.cards.blackjackdata.{BlackJackHand, DealtDeck, OpenHand, PartiallyOpenHand}

/**
  * Created by alexbate on 09/06/16.
  */
object PlayBlackjack extends App {

  val resultantDeck: DealtDeck = dealCardsInitially(BlackJackOps.freshDeck)
  interactWithPlayer(resultantDeck) match {
    case Left(s) => println(s)
    case Right(currentDeck) => dealerPlay(DealtDeck(currentDeck.remainingDeck, currentDeck.playerHand, OpenHand(currentDeck.dealerHand.cards)))
  }

  def dealCardsInitially(deck: Deck): DealtDeck = {

    def aux(deck: Deck, userCards: BlackJackHand, dealerCards: BlackJackHand): DealtDeck = {
      (userCards.size, dealerCards.size) match {
        case (2,2) => DealtDeck(deck, userCards, dealerCards)
        case (u, d) if d < u =>
          val (nextCard, remainingDeck) = BlackJackOps.dealCard(deck)
          aux(remainingDeck, userCards, dealerCards :+ nextCard)
        case _ =>
          val (nextCard, remainingDeck) = BlackJackOps.dealCard(deck)
          aux(remainingDeck, userCards :+ nextCard, dealerCards)
      }
    }

    aux(deck, OpenHand(Nil), PartiallyOpenHand(Nil))
  }

  def interactWithPlayer(deck: DealtDeck): Either[String, DealtDeck] = {
    println(deck.show)

   deck.playerHand.score match {
     case 21 => Left("CONGRATULATIONS! YOU GOT 21!")
     case i if i > 21 => Left("Whoops, too far. Dealer kicked your ass")
     case _ => {
       println("hit or stick?\n")
       io.StdIn.readLine() match {
         case s if s.contains("hit") => {
           val (nextCard, deckWithoutCard) = BlackJackOps.dealCard(deck.remainingDeck)
           interactWithPlayer(DealtDeck(deckWithoutCard, deck.playerHand :+ nextCard, deck.dealerHand))
         }
         case _ => println("sticking"); Right(deck)
       }
      }
     }
   }

  def dealerPlay(deck: DealtDeck): String = {
    println(deck.show)

    deck.dealerHand.score match {
      case 21 => "Dealer gets to keep your money!"
      case i if i < 16 =>
        val (c, d) = BlackJackOps.dealCard(deck.remainingDeck)
        dealerPlay(DealtDeck(d, deck.playerHand, deck.dealerHand :+ c))
      case _ => "I have no idea"
    }

  }
}
