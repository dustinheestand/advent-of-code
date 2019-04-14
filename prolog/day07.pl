:- use_module(library(pio)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- op(920, fy, *).
:- (dynamic dep/2).

% Parse my file!
dependencies([E|Rest]) -->
    dependency(E),
    blanks,
    dependencies(Rest).
dependencies([E]) -->
    dependency(E). 
% A dependency has this form
dependency(dep(A, B)) -->
    "Step ",
    step(A),
    " must be finished before step ",
    step(B),
    " can begin.".
% And this is the lowest level - append the char.
step(C) -->
    % So that the values match the times
    { C1#=C+4
    },
    [C1].

% Load the functions into the database in the format dep(Time1, Time2).
load_funct(Filename) :-
    once(phrase_from_file(dependencies(D), Filename)),
    forall(member(Dep, D), assertz(Dep)),
    !.

