:- use_module(library(lists)).
:- style_check(-singleton).

/* Little wizard to get everything going */
init :-
    retractall(player(X)),
    retractall(suspect(X)),
    retractall(weapon(X)),
    retractall(room(X)),
    retractall(in_hand(_,_)),
    write('Welcome to Clue!'),nl,
    write('Please enter the names players playing today.'),nl,
    write('Enter the word \'done\' to continue\'.'),nl,
    input_players,nl,nl,
    write('Please enter the suspects (characters) you\'d like available in the game.'),nl,
    write('Enter the word \'done\' to continue\'.'),nl,
    write('If you would like to play with classic suspects, enter \'default\''),nl,
    input_suspects,nl,nl,
    write('Please enter the weapons you\'d like available in the game.'),nl,
    write('Enter the word \'done\' to continue\'.'),nl,
    write('If you would like to play with classic weapons, enter \'default\''),nl,
    input_weapons,nl,nl,
    write('Please enter the rooms you\'d like available in the game.'),nl,
    write('Enter the word \'done\' to continue\'.'),nl,
    write('If you would like to play with classic rooms, enter \'default\''),nl,
    input_rooms,nl,nl,
    write('Ready to play!'),nl,nl,
    write('What is in your hand?'),nl,
    write('Enter the word \'done\' to continue\'.'),nl,
    input_hand,
    game.

/* Helper functions which loop to keep entering each type of card */
input_players :-
    write('    Player name: '),
    read(X),
    new_player(X).

input_suspects :-
    write('    Suspect name: '),
    read(X),
    new_suspect(X).

input_weapons :-
    write('    Weapon name: '),
    read(X),
    new_weapon(X).

input_rooms :-
    write('    Room name: '),
    read(X),
    new_room(X).

input_hand:-
    write('    Card: '),
    read(X),
    new_card(X).

new_player(done) :- !.
new_player(X) :- assert(player(X)),input_players.
new_suspect(done) :- !.
new_suspect(default) :- default_suspects.
new_suspect(X) :- assert(suspect(X)),input_suspects.
new_weapon(done) :- !.
new_weapon(default) :- default_weapons.
new_weapon(X) :- assert(weapon(X)),input_weapons.
new_room(done) :- !.
new_room(default) :- default_rooms.
new_room(X) :- assert(room(X)),input_rooms.
new_card(done) :- !.
new_card(X) :- player(Me),!,assert(in_hand(Me,X)),input_hand.

game :-
    !,
    write('CLUE!!').

/* All suspects, weapons, and rooms are cards in the game.
 * When you re-init, the "deck" may change. */
:- dynamic
    card/1. %what is in the deck

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

/* what could be in the envelope */
envelope(X,Y,Z) :- possible_suspect(X),possible_weapon(Y),possible_room(Z).
possible_suspect(X) :- suspect(X),  not(in_hand(_,X)).
possible_room(X)    :- room(X),     not(in_hand(_,X)).
possible_weapon(X)  :- weapon(X),   not(in_hand(_,X)).

/* what could be in someones hand innocent until proven guilty*/
hand_possibility(P,X) :- card(X), in_hand(P,X). % for some reason adding the exclusion based on other player's hands doesn't work here.
hand_possibility(P,X) :- card(X), hasnt_passed(P,X), not(in_hand(Other,X)), not(Other == P).

/* true if a player P hasn't yet passed on a card X just to compactify things */
hasnt_passed(P,X) :- not(pass(P,X,_,_)), not(pass(P,_,X,_)), not(pass(P,_,_,X)).

/* we can infer what is in someone's hand based on what they have shown and knowing what is NOT in their hand
   but defining this as a condition on in_hand causes issues of infinite looping between in_hand and hand_possibility
   so instead we keep in_hand as pure data, it is only ever asserted, not inferred.*/
infer_hand(P) :- showed(P, X, Y, Z), not(hand_possibility(P,Y)), not(hand_possibility(P,Z)), assert(in_hand(P,X)).
infer_hand(P) :- showed(P, X, Y, Z), not(hand_possibility(P,X)), not(hand_possibility(P,Z)), assert(in_hand(P,Y)).
infer_hand(P) :- showed(P, X, Y, Z), not(hand_possibility(P,X)), not(hand_possibility(P,Y)), assert(in_hand(P,Z)).

/* tells us if we know absolutely what is in someone's hand */
certain(P) :- setof(X,in_hand(P,X),Hand), setof(X,hand_possibility(P,X),Poss), permutation(Hand,Poss).
/* but we want to be able to do this also based on the size of someone's hand
   ie we can eliminate things that are not in their hand as possibilities once 
   we know they have N cards if we know their hand has size N
   But I don't really want to do arithmetic*/

card(X) :- room(X).
card(X) :- weapon(X).
card(X) :- suspect(X).

% Defaults
default_suspects :-
    assert(suspect(peacock)),
    assert(suspect(mustard)),
    assert(suspect(plum)),
    assert(suspect(scarlet)),
    assert(suspect(white)),
    assert(suspect(green)).

default_weapons :-
    assert(weapon(wrench)),
    assert(weapon(pistol)),
    assert(weapon(rope)),
    assert(weapon(knife)),
    assert(weapon(lead)),
    assert(weapon(candle)).

default_rooms :-
    assert(room(hall)),
    assert(room(conservatory)),
    assert(room(kitchen)),
    assert(room(ballroom)),
    assert(room(billiard)),
    assert(room(library)),
    assert(room(study)),
    assert(room(lounge)),
    assert(room(dining)).

def_init :-
    retractall(player(X)),
    retractall(suspect(X)),
    retractall(weapon(X)),
    retractall(room(X)),
    assert(player(a)),
    assert(player(b)),
    assert(player(c)),
    default_suspects,
    default_weapons,
    default_rooms,
    game.
