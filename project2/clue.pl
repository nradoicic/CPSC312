:- use_module(library(lists)).

/* allow us to define cards dynamically */
:- dynamic
    suspect/1.
:- dynamic
    weapon/1.
:-dynamic
    room/1. %
    
/* in_hand(Player,Card) is true when we know a card is definitely in someone's hand
   this may happen when a card is shown to us and can be inferred at certain specific points */
:- dynamic
    in_hand/2. %track what's in one's hand
/* suggested(Player,Card,Card,Card) is true when someone suggests a triple of suspect
   weapon and room (order doesn't matter) */
:- dynamic
    suggested/4. %who suggests what
/* showed(Plater,Card,Card,Card) is true when a player shows a card to another player
   to disprove a suggestion, based on other knowledge this may confirm what is in a players hand */
:- dynamic
    showed/4. %when someone shows a card for a suggestion
/* pass(Player,Card,Card,Card) is true when a player passes on disproving a suggestion
   this can be used to eliminate possibilities from their hand since to pass one must not have
   any of the suggested cards*/
:- dynamic
    pass/4. %when someone passes on a suggestion

/* define our domain

In future this needs to be dynamic and inputtable
additionally it would be nice if the number of cards was not fixed.*/
suspect(peacock).
suspect(mustard).
suspect(plum).
suspect(scarlet).
suspect(white).
suspect(green).

weapon(wrench).
weapon(pistol).
weapon(rope).
weapon(knife).
weapon(lead).
weapon(candle).

room(hall).
room(conservatory).
room(kitchen).
room(ballroom).
room(billiard).
room(library).
room(studyr).
room(lounge).
room(dining).

card(X) :- room(X).
card(X) :- weapon(X).
card(X) :- suspect(X).

/* what could be in the envelope */
envelope(X,Y,Z) :- possible_suspect(X),possible_weapon(Y),possible_room(Z).
possible_suspect(X) :- suspect(X),  not(in_hand(_,X)).
possible_room(X)    :- room(X),     not(in_hand(_,X)).
possible_weapon(X)  :- weapon(X),   not(in_hand(_,X)).

/* what could be in someones hand innocent until proven guilty*/
hand_possibility(P,X) :- card(X), in_hand(P,X),      not(in_hand(Other,X)), not(Other == P).
hand_possibility(P,X) :- card(X), hasnt_passed(P,X), not(in_hand(Other,X)), not(Other == P).

/* true if a player P hasn't yet passed on a card X just to compactify things */
hasnt_passed(P,X) :- not(pass(P,X,_,_)) ; not(pass(P,_,X,_)) ; not(pass(P,_,_,X)).

/* what is definitely in someone's hand */
in_hand(P,X) :- showed(P, X, Y, Z), not(hand_possibility(P,Y)), not(hand_possibility(P,Z)).
in_hand(P,Y) :- showed(P, X, Y, Z), not(hand_possibility(P,X)), not(hand_possibility(P,Z)).
in_hand(P,Z) :- showed(P, X, Y, Z), not(hand_possibility(P,X)), not(hand_possibility(P,Y)).

/* tells us if we know absolutely what is in someone's hand */
certain(P) :- setof(X,in_hand(P,X),Hand), setof(X,hand_possibility(P,X),Poss), permutation(Hand,Poss).
/* but we want to be able to do this also based on the size of someone's hand
   ie we can eliminate things that are not in their hand as possibilities once 
   we know they have N cards if we know their hand has size N
   But I don't really want to do arithmetic*/



