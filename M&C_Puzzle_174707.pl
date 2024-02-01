solve:- % Solve Predicate
    initial( Start), %Initial State set on line 15
    breadthfirst( [ [Start] ], Solution), %This is the main part of the proccessong. Breadth-first search (BFS) is an algorithm for searching a tree data structure for a node that satisfies a given property. 
    % Solutions is what the BFS will return as an answer
    % Solution is a path (in reverse order) from initial to a goal
    write(Solution), nl,
    printsol(Solution).
	%The solution will come out in the console.

%In order to call the BFS, we have to define the safe states (rules), that will follow our restriction to be safe with the number of Cannibals
% safe(Number_Missionaries, Number_Cannibals) is true if Number_Missionaries is 0 or 3 or equal to Number_Cannibals
safe(0, _).
safe(3, _).
safe(X, X).

% A state is represented by a term:
%	state( NumOfMissionaries, NumOfCannibals, BoatAtEast)
initial(state(3,3,1)). %Initial State, we have 3 Mis, 3 Can and the boat on the left side
goal(state(0,0,0)). % We want 0 Mis, 0 Can on the left side, and the boat on the right side
goalpath([Node | _]) :- goal(Node). % The solution state will be given if the last node in the goalpath matches with the goal node.

% Now the possible moves of the boat will be defined
% move( State1, State2): making a move in State1 results in State2;

move( state( M1, C1, 1),	%Before move
      state( M2, C1, 0) )	%After move
	:- M1 > 1, M2 is M1-2, safe(M2, C1). %Two missionaries from left to right

move( state( M1, C1, 0),	%Before move
      state( M2, C1, 1) )	%After move
	:- M1 < 2, M2 is M1+2, safe(M2, C1). %Two missionaries from right to left
         
move( state( M1, C1, 1),	%Before move
      state( M1, C2, 0) )	%After move
	:- C1 > 1, C2 is C1-2, safe(M1, C2). %Two cannibals from left to right

move( state( M1, C1, 0),	%Before move
      state( M1, C2, 1) )	%After move
	:- C1 < 2, C2 is C1+2, safe(M1, C2). %Two cannibals from right to left

move( state( M1, C1, 1),	%Before move
      state( M1, C2, 0) )	%After move
	:- C1 > 0, C2 is C1-1, safe(M1, C2). %One cannibal from left to right

move( state( M1, C1, 0),	%Before move
      state( M1, C2, 1) )	%After move
	:- C1 < 3, C2 is C1+1, safe(M1, C2). %One cannibal from right to left

move( state( M1, C1, 1),	%Before move
      state( M2, C2, 0) )	%After move
	:- M1 > 0, M2 is M1-1,	%One missionary and one cannibal from left to right
       C1 > 0, C2 is C1-1, safe(M2, C2).

move( state( M1, C1, 0),	%Before move
      state( M2, C2, 1) )	%After move
	:- M1 < 3, M2 is M1+1,	%One missionary and one cannibal from left to right
       C1 < 3, C2 is C1+1, safe(M2, C2).

%Function made to print on the console with the function explain
printsol([X]) :- write(X), write(': initial state'), nl.
printsol([X,Y|Z]) :- printsol([Y | Z]), write(x), explain(Y, X), nl.

%Function made to "explain" what move is done on each step, with the numbers of Missionaries and Cannibles with the direction of the boat
explain(state(M1, C1, 1), state(M2, C2, _)) :-
    X is M1-M2, Y is C1-C2,
    write(': '), write(X), write(' missionaries and '),
    write(Y), write(' cannibals moved from left to tight').
explain(state(M1, C1, 0), state(M2, C2, _)) :-
    X is M2-M1, Y is C2-C1,
    write(': '), write(X), write(' missionaries and '),
    write(Y), write(' cannibals moved from right to left').

% An implementation of breadth-first search

% breadthfirst( [ Path1, Path2, ...], Solution):
% each Path(i) represents [Node | Ancestors], where Node is in the open list and
% Ancestors is a path from the parent of Node to the initial node in the search tree.
% Solution is a path (in reverse order) from initial to a goal.
% We receive a set of Paths, and will return the path as a solution
% only if it is the goal path


breadthfirst( [ Path | _], Path) :-
    goalpath( Path ). %if Path is a goal-path, then it is a solution

% If it is not the goal path, then the last path saved will be taken and  
% it will be expanded to find new paths, and then append those to the already existing paths
% and then the function is already called.

breadthfirst( [Path | Paths], Solution) :-
    extend( Path, NewPaths),
    append( Paths, NewPaths, Paths1),
    breadthfirst( Paths1, Solution).

% setof(X, Condition, Set) is a built-in function: it collects all X satisfying Condition into Set.
% with the method of extend what we will do, is take the last node from the queue of paths that have 
% been acumulated and it will find, from the node we are currently, another it can move to, using the move function.
% This is done as long as the new node it moves to, is not a node already set in the existing path that we know 
% Then it will be added

extend( [Node | Path], NewPaths) :-
    setof( [NewNode, Node | Path],
           ( move( Node, NewNode), not(member( NewNode, [Node | Path] )) ),
           NewPaths),
    !.

extend( _, [] ).

not(P) :- P, !, fail.
not(_).




    



