# Blackjack
An implementation of the game Blackjack in Haskell

Player enters in their fund and their amount to bet to play against the Dealer (Computer). 
Two cards are dealt to the player while one card is dealt to the dealer.
Player is given a choice to "Hit" or "Stand", where "Hit" allows you to draw an extra card and "Stand" to let the Dealer have it's turn. 
Player wins if their hand = 21, if their hand is greater than the dealer's hand but still less than 22, or if the Dealer busts.
Player loses if their hand > 21 (Bust), or their hand is less than the dealer's hand but no more than 22.
Double-Down or Card Splitting is not implemented.
