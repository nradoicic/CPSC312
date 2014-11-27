:- use_module(library(lists)).
:- style_check(-singleton).

/* -----------------------------------------------------------------------------
   -----------------------------Dynamic Actions---------------------------------
   ---------------------------------------------------------------------------*/

/* room, weapon, and suspect are the three types of cards which may be defined for a game of clue */
:- dynamic
    room/1. % where the murder happened
:- dynamic
    weapon/1. % how the murder happened
:- dynamic
    suspect/1. % who did the murdering

/* players are also defined at runtime */
:- dynamic
    player/1. % the players are distinct from the suspects

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
infer_hands   :- findall(P,(player(P),infer_hand(P)),_).
infer_hand(P) :- infer_hand1(P),infer_hand2(P),infer_hand3(P),infer_hand4(P).
/* These all need to return true so that we are guaranteed that all the asserts may be reached. */
infer_hand1(P) :- showed(P, X, Y, Z), not(hand_possibility(P,Y)), not(hand_possibility(P,Z)), add_to_hand(P,X).
infer_hand2(P) :- showed(P, X, Y, Z), not(hand_possibility(P,X)), not(hand_possibility(P,Z)), add_to_hand(P,Y).
infer_hand3(P) :- showed(P, X, Y, Z), not(hand_possibility(P,X)), not(hand_possibility(P,Y)), add_to_hand(P,Z).
/* additionally if a player's hand size is the same as the size of their possiblities we may add all possibilities to their hand */
infer_hand4(P) :- hand_size(P,Size), find_unique(X,hand_possibility(P,X),Poss), length(Poss,Size), find_unique(X,(hand_possibility(P,X),add_to_hand(P,X)),_).

/* helper to assert what is in someone's hand without adding duplicates */
add_to_hand(Player,Card) :- in_hand(Player,Card),!; assert(in_hand(Player,Card)),!;true.


/* -----------------------------------------------------------------------------
   -----------------------------Interface Code----------------------------------
   ---------------------------------------------------------------------------*/

clue :- init.
/* Little wizard to get everything going */
init :-
    clean_state,
    banner,
    instructions,
    writeln('Please enter the names of players playing today (starting with'),
    writeln('your name).  Enter the word \'done\' once all players are entered\'.'),
    input_players,nl,nl,
    writeln('How many cards does each player have? If you are uncertain, enter'),
    writeln('the maximum number of cards which may be in their hand.'),
    findall(Player,
        (player(Player),
        write('    '),write(Player),write(' : '),
        read(Hand_size),
        assert(hand_size(Player,Hand_size))),
    _),nl,
    writeln('Please enter the suspects (characters) you\'d like to play with.'),
    writeln('Enter the word \'done\' to continue\'.'),
    writeln('If you would like to play with classic suspects, enter \'default\''),
    input_suspects,nl,nl,
    writeln('Please enter the weapons you\'d like to play with.'),
    writeln('Enter the word \'done\' to continue\'.'),
    writeln('If you would like to play with classic weapons, enter \'default\''),
    input_weapons,nl,nl,
    writeln('Please enter the rooms you\'d like to play with.'),
    writeln('Enter the word \'done\' to continue\'.'),
    writeln('If you would like to play with classic rooms, enter \'default\''),
    input_rooms,nl,nl,
    writeln('Please enter the cards in your hand.'),
    writeln('Enter the word \'done\' to continue\'.'),
    input_hand,
    nl,writeln('****** Ready to play! ******'),nl,
    writeln('------------------------------------------------------------------'),
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

/* Sweet ASCII banner */
banner :-
    nl,
    writeln('                 ________    __  __________'),
    writeln('                / ____/ /   / / / / ____/ /'),
    writeln('               / /   / /   / / / / __/ / /'),
    writeln('              / /___/ /___/ /_/ / /___/_/'),
    writeln('              \\____/_____/\\____/_____(_)\n'),
    writeln('------------------------------------------------------------------\n').

/* instructions on how to input data */
instructions :-
    writeln('Welcome to Clue Helper!\n'),
    writeln('The program will track the data from a game of clue and will make'),
    writeln('logical inferences based on the passage of the game.  The program'),
    writeln('will tell you what it knows is in each player\'s hand, as well as'),
    writeln('all cards which may be in each player\'s hand.  The program will'),
    writeln('tell you which cards may be in the envelope.\n'),
    writeln('When inputting data ensure that the name of a card or player is in'),
    writeln('lower case and does not contain any punctuation, end each entry'),
    writeln('with a period.\n'),
    writeln('When you are done entering data to a prompt, or if a prompt does'),
    writeln('not apply enter \'done.\'\n'),
    writeln('------------------------------------------------------------------').

/* Game loop
   runs through the steps of the game
   currently no accusations, not sure what to do with those anyway
   doesn't output anything useful yet, need to know when to ask
   we can dump data all the time whatevs.*/
game :-
    get_time(A),atom_number(C,A),atom_string(A,B),qsave_program(B), % save the current game state
    infer_hands_settle,     
    print_state,
    writeln('Which player is making a suggestion?'),
    write('    Player name: '),read(Player),nl,
    writeln('What three cards did they suggest?'),
    write('    Card name:   '),read(X),
    write('    Card name:   '),read(Y),
    write('    Card name:   '),read(Z),nl,
    writeln('Which players passed?'),
    pass_loop(X,Y,Z),nl,
    writeln('Which player showed a card?'),
    write('    Player name: '),read(Shower),nl,
    writeln('If they showed you a card what was it? (if not, enter \'done\')'),
    write('    Card name:   '),read(Card_shown),nl, 
    showed_me(Shower, Card_shown),
    assert(showed(Shower,X,Y,Z)), % think hard about when it comes full circle
    writeln('------------------------------------------------------------------'),nl,
    game.

/* predicate which will perform inferences on the history of the game until no more can be performed */
infer_hands_settle :- findall(X,in_hand(_,X),L), infer_hands, findall(X,in_hand(_,X),NL),(NL==L,!;infer_hands_settle).

/* helper which records what's in the hand of a player who shows us a card */
showed_me(_,done) :- true.
showed_me(Player,Card) :- add_to_hand(Player,Card).

/* helper which records who passes on a suggestion */
pass_loop(X,Y,Z) :-
    write('    Player name: '),
    read(Player),
    record_pass(Player,X,Y,Z).

record_pass(done,_,_,_) :- !.
record_pass(P,X,Y,Z) :-
    assert(pass(P,X,Y,Z)),
    pass_loop(X,Y,Z).

/* dumps out our current notes
*  this is done at the start of each round */
print_state :-
    nl,writeln('Each player\'s definite hand and possible cards in hand:'),nl,
    findall(Player,
        (
            player(Player),
            setof(X,hand_possibility(Player,X),L),
            write('Player: '),writeln(Player),
            write('    Possibilities : '),writeln(L),setof(X,in_hand(Player,X),Hand),
            write('    Hand          : '),writeln(Hand),nl;true
        ),_),nl,
    writeln('------------------------------------------------------------------'),nl,
    nl,writeln('The possible cards which may be in the envelope:'),
    write('    Suspects: '),
    findall(Suspect,
        (
            possible_suspect(Suspect)
        ),S),
    write(S),nl,
    findall(Weapon,
        (
            possible_weapon(Weapon)
        ),W),
    write('    Weapons:  '),write(W),nl,
    findall(Room,
        (
            possible_room(Room)
        ),R),
    write('    Rooms:    '),write(R),nl,
    nl,writeln('------------------------------------------------------------------'),nl.

/* default initializations
*  If the user would like to play with standard clue pieces this will make the appropriate asserts. */
default_suspects :-
    assert(suspect(peacock)),
    assert(suspect(mustard)),
    assert(suspect(plum)),
    assert(suspect(scarlet)),
    assert(suspect(white)),
    assert(suspect(green)),
    nl,writeln('The suspects are: '),
    findall(X,(suspect(X),write('    '),writeln(X)),_).

default_weapons :-
    assert(weapon(wrench)),
    assert(weapon(pistol)),
    assert(weapon(rope)),
    assert(weapon(knife)),
    assert(weapon(lead)),
    assert(weapon(candle)),
    nl,writeln('The weapons are: '),
    findall(X,(weapon(X),write('    '),writeln(X)),_).

default_rooms :-
    assert(room(hall)),
    assert(room(conservatory)),
    assert(room(kitchen)),
    assert(room(ballroom)),
    assert(room(billiard)),
    assert(room(library)),
    assert(room(study)),
    assert(room(lounge)),
    assert(room(dining)),
    nl,writeln('The rooms are: '),
    findall(X,(weapon(X),write('    '),writeln(X)),_).

def_init :-
    clean_state,
    assert(player(a)),
    assert(player(b)),
    assert(player(c)),
    assert(hand_size(a,6)),
    assert(hand_size(b,6)),
    assert(hand_size(c,6)),
    default_suspects,
    default_weapons,
    default_rooms,
    instructions,
    game.

/* resets the game world */
clean_state :-
    retractall(player(_)),
    retractall(suspect(_)),
    retractall(weapon(_)),
    retractall(room(_)),
    retractall(in_hand(_,_)),
    retractall(hand_size(_,_)),
    retractall(pass(_,_,_,_)),
    retractall(suggested(_,_,_,_)),
    retractall(showed(_,_,_,_)).
