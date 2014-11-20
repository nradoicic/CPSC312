:- use_module(library(lists)).
:- style_check(-singleton).

/* -----------------------------------------------------------------------------
   -----------------------------Dynamic Actions---------------------------------
   ---------------------------------------------------------------------------*/
%

/* in_hand(Player,Card) is true when we know a card is definitely in someone's hand
   this may happen when a card is shown to us and can be inferred at certain specific points */
:- dynamic
    in_hand/2. %track what's in one's hand
    
/* hand_size(Player,Int) tracks the number of cards in a player's hand. This is used to 
   infer when we're done determining what's in a players hand. */
:- dynamic
    hand_size/2. %track what's in one's hand

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

/* -----------------------------------------------------------------------------
   ------------------------------Logic Engine-----------------------------------
   ---------------------------------------------------------------------------*/
%
/* cards are either rooms, weapons or suspects */
card(X) :- room(X).
card(X) :- weapon(X).
card(X) :- suspect(X).

/* what could be in the envelope */
envelope(X,Y,Z) :- possible_suspect(X), possible_weapon(Y), possible_room(Z).

/* something could be in the envelope if it's not in anyone's hand 
   ignore the possibilities if we know exactly what is in the envelope */
possible_suspect(X) :- definite_suspect(X),!; suspect(X),  not(in_hand(_,X)).
possible_room(X)    :- definite_room(X),!;    room(X),     not(in_hand(_,X)).
possible_weapon(X)  :- definite_weapon(X),!;  weapon(X),   not(in_hand(_,X)).

/* something is definitely in the envelope if it is definitely not in anyone's hand */
definite_suspect(X) :- suspect(X), not((player(P), hand_possibility(P,X))).
definite_room(X)    :- room(X),    not((player(P), hand_possibility(P,X))).
definite_weapon(X)  :- weapon(X),  not((player(P), hand_possibility(P,X))).

/* what could be in someones hand
   something could be in someone's hand if it is in their hand
   something could be in someone's hand if it's not in anyone else's hand and they haven't passed on it
   additionally once we know everything in someone's hand we don't need to consider any extra possibilities */
hand_possibility(P,X) :- card(X), in_hand(P,X). % for some reason adding the exclusion based on other player's hands doesn't work here.
hand_possibility(P,X) :- not_maxed(P),card(X), hasnt_passed(P,X), not(in_hand(Other,X)), not(Other == P).

/* determines if we've figured out every card in a players hand based on the known size of their hand */
not_maxed(P) :- find_unique(X,in_hand(P,X),Hand), hand_size(P,Size), not(length(Hand,Size)).
/* setof results in false instead of [] if nothing satisfies the goal, but we want [] with no duplicates */
find_unique(X,Goal,L) :- setof(X,Goal,L),!;L=[].

/* true if a player P hasn't yet passed on a card X just to compactify things */
hasnt_passed(P,X) :- not(pass(P,X,_,_)), not(pass(P,_,X,_)), not(pass(P,_,_,X)).

/* we can infer what is in someone's hand based on what they have shown and knowing what is NOT in their hand
   but defining this as a condition on in_hand causes issues of infinite looping between in_hand and hand_possibility
   so instead we keep in_hand as pure data, it is only ever asserted, not inferred. 
   this predicate should always return true otherwise the game loop may not continue properly*/
infer_hand(P) :- infer_hand1(P),infer_hand2(P),infer_hand3(P).
/* These all need to return true so that we are guaranteed that all the asserts may be reached. */
infer_hand1(P) :- showed(P, X, Y, Z), not(hand_possibility(P,Y)), not(hand_possibility(P,Z)), assert(in_hand(P,X));true.
infer_hand2(P) :- showed(P, X, Y, Z), not(hand_possibility(P,X)), not(hand_possibility(P,Z)), assert(in_hand(P,Y));true.
infer_hand3(P) :- showed(P, X, Y, Z), not(hand_possibility(P,X)), not(hand_possibility(P,Y)), assert(in_hand(P,Z));true.


/* -----------------------------------------------------------------------------
   -----------------------------Interface Code----------------------------------
   ---------------------------------------------------------------------------*/
%

/* Little wizard to get everything going */
init :-
    clean_state,
    write('Welcome to Clue!'),nl,
    write('Please enter the names players playing today.'),nl,
    write('Enter the word \'done\' to continue\'.'),nl,
    input_players,nl,nl,
    write('How many cards does each player have.'),nl,
    findall(Player,
        (player(Player),
        write(Player),
        write(' : '),
        read(Hand_size),
        assert(hand_size(Player,Hand_size))),
    _),nl,
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

/* helpers to record game state or break out the loop */
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

/* Game loop
   runs through the steps of the game
   currently no accusations, not sure what to do with those anyway
   doesn't output anything useful yet, need to know when to ask
   we can dump data all the time whatevs.*/
game :-
    print_state,
    write('Whose turn is it?      : '),
    read(Player),
    write('What did they suggest? : '),
    read(X),
    read(Y),
    read(Z),
    write('Who passed?'),nl,
    pass_loop(X,Y,Z),
    write('Who showed a card?     : '),
    read(Shower),
    write('Showed me: '),
    read(Card_shown),
    showed_me(Shower, Card_shown),
    infer_hand(Shower),
    assert(showed(Shower,X,Y,Z)), % think hard about when it comes full circle
    nl,writeln('---------------------------------------'),
    game.

/* helper which records what's in the hand of a player who shows us a card */
showed_me(_,done) :- true.
showed_me(Player,Card) :- assert(in_hand(Player,Card)).

/* helper which records who passes on a suggestion */
pass_loop(X,Y,Z) :-
    write('    Player name:       : '),
    read(Player),
    record_pass(Player,X,Y,Z).

record_pass(done,_,_,_) :- !.
record_pass(P,X,Y,Z) :-
    assert(pass(P,X,Y,Z)),
    infer_hand(P),
    pass_loop(X,Y,Z).
    
/* dumps out our current notes */
print_state :-
    findall(Player,
        (
            player(Player),
            setof(X,hand_possibility(Player,X),L),
            write('Player: '),writeln(Player),
            write('    Possibilities : '),writeln(L),setof(X,in_hand(Player,X),Hand),
            write('    Hand          : '),writeln(Hand);true
        ),_),nl,
    writeln('---------------------------------------'),
    nl,
    write('Suspects: '),
    findall(Suspect,
        (
            possible_suspect(Suspect)
        ),S),
    write(S),nl,
    findall(Weapon,
        (
            possible_weapon(Weapon)
        ),W),
    write('Weapons:  '),write(W),nl,
    findall(Room,
        (
            possible_room(Room)
        ),R),
    write('Rooms:    '),write(R),nl.

/* default initializations */
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
    clean_state,
    assert(player(a)),
    assert(player(b)),
    assert(player(c)),
    default_suspects,
    default_weapons,
    default_rooms,
    game.
    
/* resets the game world */
clean_state :- 
    retractall(player(_)),
    retractall(suspect(_)),
    retractall(weapon(_)),
    retractall(room(_)),
    retractall(in_hand(_,_)). 