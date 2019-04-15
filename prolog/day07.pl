:- use_module(library(pio)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- op(920, fy, *).
:- (dynamic prereq_dependent/2).

% Parse my file!
dependencies([E|Rest]) -->
    dependency(E),
    blanks,
    dependencies(Rest).
dependencies([E]) -->
    dependency(E). 
% A dependency has this form
dependency(prereq_dependent(A, B)) -->
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

get_order(Order) :-
    solve(Ints),
    maplist(plus(4), Ints, Codes),
    text_to_string(Codes, Order).

% Now to actually solve the problem
solve(Ordering) :-
    findall(Prereq, prereq_dependent(Prereq, _), Prereqs),
    findall(Dependent, prereq_dependent(_, Dependent), Dependents),
    ord_union(Prereqs, Dependents, Tasks),
    sort(Tasks, SortedTasks),
    elves_init(5, Elves),
    phrase(solve(Elves, [], SortedTasks, []), Ordering).

elves_init(0, []).
elves_init(N, [(0, -1)|Es]) :-
    N#>0,
    N1#=N-1,
    elves_init(N1, Es).

% In the case where there is an open slot and an eligible task
% to fill it.
solve(Elves, InProgress, ToDo, Completed) -->
    { select((0, N), Elves, Elves1),
      N#<0,
      member(Task, ToDo),
      % Inelegant - I'd like to take the complement of Completed
      append(InProgress, ToDo, NotCompleted),
      \+ dependent(NotCompleted, Task),
      select(Task, ToDo, ToDo1)
    },
    [Task],
    solve([(Task, Task)|Elves1],
          [Task|InProgress],
          ToDo1,
          Completed).

% If a task is finished, there might be more!
% So just move it to Completed and set the elf to (0,-1)
solve(Elves, InProgress, ToDo, Completed) -->
    { select((FinishedTask, 0), Elves, Elves1),
      select(FinishedTask, InProgress, InProgress1)
    },
    [],
    solve([(0, -1)|Elves1], InProgress1, ToDo, [FinishedTask|Completed]).

% If we find we can't add anything, just decrement the time for each elf
solve(Elves, InProgress, ToDo, Completed) -->
    { dif([], InProgress),
      append(InProgress, ToDo, NotCompleted),
      % If all the tasks not yet completed are dependent on ToDos
      (   maplist(dependent(NotCompleted), ToDo)
      ;   maplist(has_time_left, Elves)
      ),
      subtract_time(Elves, Elves1)
    },
    [],
    solve(Elves1, InProgress, ToDo, Completed).

solve(_, [], [], _) -->
    [].

dependent(Prereqs, Task) :-
    member(Prereq, Prereqs),
    prereq_dependent(Prereq, Task).

subtract_time(Elves, Elves1) :-
    min_time_left(Elves, Min),
    maplist(subtract_time(Min), Elves, Elves1).

min_time_left(Elves, N) :-
    maplist(get_time_left, Elves, Times),
    min_list(Times, N).

% Need to give a silly value for negative numbers
% As I'm using them to signify empty slots
get_time_left((_, N), N) :-
    N#>=0.
get_time_left((_, N), 1000000) :-
    N#<0.

subtract_time(T,  (M, N),  (M, N1)) :-
    N1#=N-T.

has_time_left((_, N)) :-
    N#>0.