:-["input.txt"].
:-use_module(library(clpfd)).
%Reference: https://coderwall.com/p/laduzw/how-to-measure-execution-time-in-swi-prolog


start("no_random") :-
    %START FUNCTION
    %Use data from input.txt file
    %Execute BACKTRACKING algorithm
    statistics(walltime, [_ | [_]]),
    size(N), K is N*N,
    backtrack(K),
    statistics(walltime, [_ | [ExecutionTime]]),
    write('Execution took '), write(ExecutionTime), write(' ms.'), nl.


start("random") :-
    %START FUNCTION
    %Create random map and save it in input.txt
    %Execute BACKTRACKING algorithm
    random,
    statistics(walltime, [_ | [_]]),
    size(N), K is N*N,
    backtrack(K),
    statistics(walltime, [_ | [ExecutionTime]]),
    write('Execution took '), write(ExecutionTime), write(' ms.'), nl.

%Check that two cells are adjacent to each other.
adjacent(cell(X,Y),cell(X1,Y)):- X1 #= X+1, cell(X,Y),cell(X1,Y) .
adjacent(cell(X,Y),cell(X1,Y)):- X1 #= X-1, cell(X,Y),cell(X1,Y).
adjacent(cell(X,Y),cell(X,Y1)):- Y1 #= Y+1, cell(X,Y),cell(X,Y1).
adjacent(cell(X,Y),cell(X,Y1)):- Y1 #= Y-1, cell(X,Y),cell(X,Y1).
adjacent(cell(X,Y),cell(X1,Y1)):- X1 #= X+1, Y1 #= Y+1, cell(X,Y),cell(X1,Y1).
adjacent(cell(X,Y),cell(X1,Y1)):- X1 #= X-1, Y1 #= Y-1, cell(X,Y),cell(X1,Y1).
adjacent(cell(X,Y),cell(X1,Y1)):- X1 #= X-1, Y1 #= Y+1, cell(X,Y),cell(X1,Y1).
adjacent(cell(X,Y),cell(X1,Y1)):- X1 #= X+1, Y1 #= Y-1, cell(X,Y),cell(X1,Y1).

%Create coordinates(facts) of cells 
cell(X,Y):-
    size(S),
    X in 1..S,
    Y in 1..S. 
    
%Create coordinate of agent like (1,n), where n is the size of the map
agent(cell(X,Y)):-
    size(S),
    X is 1,
    Y is S.

%Just changing of notation
home(cell(X,Y)):- home(X,Y).
doctor(cell(X,Y)):- doctor(X,Y).
mask(cell(X,Y)):- mask(X,Y).
covid(cell(X,Y)):-
    adjacent(cell(X,Y),cell(X1,Y1)),
    covid(X1,Y1);
    covid(X,Y).
% [Cell, DoctorMask, Array]
% 'Cell' - cell which we consider.
% DoctorMask - whether we visited doctor or mask sells (boolean).
% Array - is array of all visited cells.

backtrack(MaxLen):-
    agent(cell(X,Y)),
    make_step([FoundN, MaxLen], [cell(X,Y), false, [cell(X,Y)]],_) -> 
    backtrack(FoundN), !; false.

backtrack(MaxLen):-
    agent(cell(X,Y)),
    make_step([cell(X,Y), false, [cell(X,Y)]], _, MaxLen),
    !.

% Make step and see whether it is better length or not.
make_step([FoundLen, PrevLen], [Cell1, DoctorMask, Array1], [Cell2, DoctorMask, Array2]):-
    step([Cell1, DoctorMask, Array1], [Cell2, DoctorMask, Array2]),
    \+ home(Cell2),
    
    length(Array2, T),
    K is T-1,
    PrevLen #> K,

    make_step([FoundLen, PrevLen], [Cell2, DoctorMask, Array2], _), 
    !.

%if there is a home
make_step([FoundLen, PrevLen], [Cell1, DoctorMask, Array1], [Cell2, DoctorMask, Array2]):-
    home(Cell2),
    step([Cell1, DoctorMask, Array1], [Cell2, DoctorMask, Array2]),

    length(Array2,T),
    K is T-1,
    FoundLen = K,
    PrevLen #> K,
    
    append(Array1, [Cell2], Array2),
    !.

make_step(P1, P2,Len):-
    step(P1,P2),
    P2 = [Cell2, _, Array2],
    \+ home(Cell2),

    length(Array2, T),
    K is T-1,
    Len #> K,

    make_step(P2, _, Len), 
    !.

make_step([Cell1, DoctorMask, Array1], [Cell2, DoctorMask, Array2], Len):-
    home(Cell2),
    step([Cell1, DoctorMask, Array1], [Cell2, DoctorMask, Array2]),

    length(Array2,T),
    K is T-1,
    Len #> K,

    append(Array1, [Cell2], Array2),
    write('WIN'),nl,
    write('Number of steps: '), write(K), nl,
    write('Path: '), write(Array2),nl,
    !.

% If cell is home.
step([Cell1, DoctorMask, Array1], [Cell2, DoctorMask, Array2]):-
    home(Cell2),
    adjacent(Cell1,Cell2),
    append(Array1, [Cell2], Array2), 
    !.

% If cell is doctor or mask.
step([Cell1, _, Array1], [Cell2, true, Array2]):-
    (mask(Cell2);doctor(Cell2)),
    adjacent(Cell1,Cell2),
    \+ subset([Cell2], Array1),
    append(Array1, [Cell2], Array2).

%If cell is covid, but we have the mask or visited doctor or empty cell.
step([Cell1, DoctorMask, Array1], [Cell2, DoctorMask, Array2]):-
    adjacent(Cell1,Cell2),
    (
        (covid(Cell2) -> false; (true)); 
        DoctorMask== true
    ),
    \+ (mask(Cell2);doctor(Cell2)),
    \+ subset([Cell2], Array1),
    append(Array1, [Cell2], Array2).
bfs_start("no_random") :-
    %START FUNCTION
    %Use data from input.txt file
    %Execute BFS algorithm
    statistics(walltime, [_| [_]]),
    bfs,
    statistics(walltime, [_ | [ExecutionTime]]),
    write('Execution took '), write(ExecutionTime), write(' ms.'), nl.
bfs_start("random") :-
    %START FUNCTION
    %Create random map and save it in input.txt
    %Execute BFS algorithm
    random,
    statistics(walltime, [_ | [_]]),
    bfs,
    statistics(walltime, [_ | [ExecutionTime]]),
    write('Execution took '), write(ExecutionTime), write(' ms.'), nl.

rand_cell(N, P):-
    random_between(1, N, X),
    random_between(1, N, Y),
    P = cell(X,Y).
random:-
    (random_one -> true,!;random).
random_one:-
    %Generate random map
    abolish(size/1),
    abolish(home/2),
    abolish(covid/2),
    abolish(doxtor/2),
    abolish(mask/2),

    random_between(6, 9, X),
    assert(size(X)),
    
    rand_cell(X, Cov1),
    Cov1 = cell(X1,Y1), 
    Cov1 \= cell(1,X), 
    \+ adjacent(cell(1,X),Cov1),
    assert(covid(X1,Y1)),
    

    rand_cell(X, Cov2),
    Cov2 = cell(X2,Y2), 
    Cov2 \= cell(1,X),
    Cov2 \= Cov1,
    \+ adjacent(cell(1,X),Cov2),
    \+ adjacent(Cov1,Cov2),
    assert(covid(X2,Y2)),
    

    rand_cell(X, Home),
    Home = cell(X3,Y3), 
    Home \= cell(1,X),
    Home \= Cov1,
    Home \= Cov2,
    \+ adjacent(Cov1,Home),
    \+ adjacent(Cov2,Home),
    assert(home(X3,Y3)),
    

    rand_cell(X, Doctor),
    Doctor = cell(X4,Y4), 
    Doctor \= cell(1,X),
    Doctor \= Cov1,
    Doctor \= Cov2,
    Doctor \= Home,
    \+ adjacent(Cov1,Doctor),
    \+ adjacent(Cov2,Doctor),
    assert(doctor(X4,Y4)),
    

    rand_cell(X, Mask),
    Mask = cell(X5,Y5), 
    Mask \= cell(1,X),
    Mask \= Cov1,
    Mask \= Cov2,
    Mask \= Home,
    Mask \= Doctor,
    \+ adjacent(Cov1,Home),
    \+ adjacent(Cov2,Home),
    assert(mask(X5,Y5)),
    write("Size: "), write(X),nl,
    write("Covid: "), write(Cov1),nl,
    write("Covid: "), write(Cov2),nl,
    write("Home: "), write(Home),nl,
    write("Doctor: "), write(Doctor),nl,
    write("Mask: "), write(Mask),nl,!.
