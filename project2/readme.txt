Jaxsun McCarthy Huggan  - k8g7
Nikola Radoicic         - z9k7

Clue Project CPSC 312

Instructions for operation:
    The program may be run by invoking the predicate "init." or "clue."
    
1. Initialization
    
    The user must enter the names of all players playing, there are traditionally 6 players in a game of Clue however the program works with an arbitrary number of players.
        Players are entered one at a time, each entry is concluded with a period.
        once all players are entered the user must enter "done."
    The user must then enter the number of cards in each player's hand.
        If the size of a particular player's hand is unknown then the maximum possible hand size should be entered.
    The user must then select the suspects, weapons and rooms used in the game.
        At this point the user may enter "default." to select the default suspects, weapons, or rooms.
        The default suspects are: plum, mustard, scarlet, peacock, white and green.
        The default weapons are: lead, pistol, knife, candle, wrench and rope.
        The default rooms are: hall, dining, billiard, ballroom, study, lounge, kitchen, conservatory, and library.
    Once all this is established the game may commence.
    
2. Playing the game
    
    Every round the game will display which cards it knows is in each player's hand as well as a list of cards which may potentially be in each player's hand.
    The game will also display a list of possible suspects, rooms and weapons which may be in the envelope.
    
    The game will then prompt user to enter the player who is making a suggestion, as well as the combination of cards which are being suggested.
    If no player makes a suggestion during a turn no action is needed by the user.
    Once a suggestion is established the game will prompt for players who passed on this suggestion, once all players are entered the user should enter "done."
    After this the program will prompt for the name of the player who showed the suggesting player a card. 
        If all players (except the suggesting player) passed on the suggestion then the user should enter the name of the player who made the suggestion.
    The program will then prompt the user to enter the name of a card if they were shown a card (as would happen if the user was the player who made the suggestion)
        If the user did not make the suggestion simply enter done.
        
    This concludes the proceedings of a round, after this the program will re-evaluate the state of the game and move on to the next round.
    
3. Inferences
    
    After and during each round the program will make logical inferences to determine what is in each player's hand and what is in the envelope.
    For clarity a card which is definitely in a player's hand also counts as possibly being in a player's hand.
    
    Rules for knowing what is in a player's hand:
        * The user knows their own hand exactly
        * When a player shows the user a card they know that player has the shown card in their hand
        * When a player has a card in their hand, this card cannot possibly be in another player's hand or in the envelope
        * When a player passes on a suggestion none of the cards in the suggestion may be in that player's hand
        * When a player shows a card to another player at least one of the suggested cards must be in the showing player's hand
            * if only one of the cards in a suggestion may be in a showing player's hand then the third card must be in the showing player's hand
        * If a the number of known cards in a player's hand matches exactly the size of the players hand no other cards could possibly be in that player's hand.
        * If the number of possible cards which could be in a player's hand matches exactly the size of the player's hand all these cards must be in a player's hand.
        
    Rules for knowing what is in the envelope:
        * A card which is in a player's hand may not be in the envelope.
        * A card which could not possibly be in any player's hand must be in the envelope.
        * If a card of a type (room, suspect, weapon) must be in the envelope no other card of that type may be in the envelope.
        
4. Extra features