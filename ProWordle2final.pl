
is_category(X):-
	word(_,X).
categories(L):-
	setof(X,is_category(X),L).
available_length(L):-
	word(X,_), string_length(X,L).
pick_word(W,L,C):-
	word(W,C),string_length(W,L).
correct_letters(L1,L2,CL):-
	intersect(L1,L2,CL).
correct_positions([],[],[]).
correct_positions([H|T],[H|T2],[H|PL]):-
	correct_positions(T,T2,PL).
correct_positions([H1|T],[H2|T2],PL):-
	H1\=H2, correct_positions(T,T2,PL).
intersect([X|Y],M,Z) :- (member(X,Y);\+ member(X,M)), intersect(Y,M,Z),!.	
intersect([X|Y],M,[X|Z]) :- member(X,M), intersect(Y,M,Z).
intersect([],_,[]).

build_kb:-
	write('Please enter a word and its category on separate lines:'), nl,
	read(X),
	( X = done;
	read(Y),
	assert(word(X,Y)),
	build_kb
	
).

play:-
	write('The available categories are: '), categories(Z),
	write(Z), nl, chooseCategory(C), pick_word(W,L,C),
	atom_chars(W,W1), L2 is L+1 ,checkGuess(L,L2,W1).
	
checkGuess(_,0,_):-
nl,
write('You lost!'),!.
checkGuess(L,1,W1):-
	write('Enter a word composed of '),
	write(L), 
	write(' letters: '),nl, read(M),(((\+string_length(M,L), write('Word is not composed of '), write(L) ,write(' letters. Try again. '));(\+word(M,_),write('Word is not in the knowledge base')),nl,
	write('Remaining Guesses are '), write(1),nl,checkGuess(L,1,W1));
	atom_chars(M,M1),
	((correct_positions(W1,M1,W1),write('You won!'));
	(correct_letters(W1,M1,_), 
	 
	checkGuess(L,0,W1)))).

checkGuess(L,L2,W1):-
	L2\==1,
	write('Enter a word composed of '),
	write(L), 
	write(' letters: '),nl, read(M),(((\+string_length(M,L), write('Word is not composed of '), write(L) ,write(' letters. Try again. '));(\+word(M,_),write('Word is not in the knowledge base')),nl,
	write('Remaining Guesses are '), write(L2),nl,checkGuess(L,L2,W1));
	atom_chars(M,M1),
	((correct_positions(W1,M1,W1),write('You won!'));
	(correct_letters(W1,M1,CL), 
	write('Correct letters are: '),
	write(CL), nl,
	write(' Correct letters in correct positions are: '),
	correct_positions(W1,M1,CP),
	write(CP),nl ,L3 is L2-1, write('Remaining Guesses are '), write(L3), nl, 
	checkGuess(L,L3,W1)))).

chooseCategory(C):-
	write('Choose a category: '),nl, 
	read(C),is_category(C),chooseLength(C),!.		
chooseCategory(_) :-
	write('This category does not exist. '),nl, 
	chooseCategory(_).

chooseLength(C):-
	write('Choose a length: '), nl , 
	read(L), ((((\+number(L),write('Please enter an integer '));(\+pick_word(_,L,C),write('There are no words of this length. '))),
	nl,chooseLength(C));
	pick_word(_,L,C),write('Game started. You have '),L2 is L+1, 
	write(L2), write(' guesses.'),nl). 
	

main:-
	build_kb,play.
	
	
