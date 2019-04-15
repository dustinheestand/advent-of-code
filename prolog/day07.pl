:- use_module(library(pio)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- op(920, fy, *).
:- (dynamic prereq_dependent/2).

% To solve, run
% load_funct("day07input.txt").
% total_time(T).

% Funny story - this oughtn't to have been solved with a DCG
% because I only need to accumulate time, not store the list of
% my steps. However, since I started by trying to get a list of steps
% (which is CFNRMLOTKWAHPBJSYZVGUQXIDE btw), I just took the easy road
% and modified my program to record the time in between each new step, rather 
% than the step itself. Then I could just sum the steps.

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

% Now to actually solve the problem
total_time(T) :-
    solve(Steps),
    sum_list(Steps, T).

% If you change the DCG to record task on line 78 and [] on line 103
% you can get the actual order!
get_order(Order) :-
    solve(Ints),
    maplist(plus(4), Ints, Codes),
    text_to_string(Codes, Order).
    
solve(TimeSteps) :-
    numlist(61, 86, Tasks),
    elves_init(5, Elves),
    phrase(solve(Elves, [], Tasks, []), TimeSteps).

elves_init(0, []).
elves_init(N, [(0, -1)|Es]) :-
    N#>0,
    N1#=N-1,
    elves_init(N1, Es).

% In the case where there is an open slot and an eligible task
% to fill it.
solve(Elves, InProgress, ToDo, Completed) -->
    { select((0, _), Elves, Elves1),
      member(Task, ToDo),
      % Inelegant - I'd like to take the complement of Completed
      append(InProgress, ToDo, NotCompleted),
      \+ dependent(NotCompleted, Task),
      select(Task, ToDo, ToDo1)
    },
    [],
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
      subtract_time(Elves,  (Elves1, Min))
    },
    [Min],
    solve(Elves1, InProgress, ToDo, Completed).

solve(_, [], [], _) -->
    [].

dependent(Prereqs, Task) :-
    member(Prereq, Prereqs),
    prereq_dependent(Prereq, Task).

subtract_time(Elves,  (Elves1, Min)) :-
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