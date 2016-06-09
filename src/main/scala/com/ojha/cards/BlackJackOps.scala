package com.ojha.cards

import com.ojha.cards.blackjackdata.BlackJackHand
import data._

/**
  * Created by alexbate on 09/06/16.
  */
object BlackJackOps {

  def freshDeck: Deck = {
    Deck(for (
      rank <- Ranks.All;
      suit <- Suits.ALL
    ) yield Card(suit, rank))
  }

  def dealCard(deck: Deck): (Card, Deck) = {
    val r = scala.util.Random
    val randomIndex = r.nextInt(deck.cards.length)
    val card = deck.cards(randomIndex)

    (card, Deck(deck.cards.slice(0, randomIndex) ++ deck.cards.slice(randomIndex + 1, deck.cards.length)))
  }


}
