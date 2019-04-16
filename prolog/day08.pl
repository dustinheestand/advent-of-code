:- use_module(library(pio)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- op(920, fy, *).
*_.
:- (dynamic make_list/1).

solve(S, Val):-
    load_funct("day08input.txt"),
    make_list(List),
    phrase(node(S,Val), List).

load_funct(Filename) :-
    once(phrase_from_file(nums(D), Filename)),
    assertz(make_list(D)),
    !.

% DCG to parse the file to a list
nums([N|Ns]) -->
    num_from_file(N1),
    space,
    nums(Ns),
    {
        number_string(N, N1)
    }.

nums([N]) -->
    num_from_file(N1),
     {
        number_string(N, N1)
    }.

num_from_file([D|Ds]) -->
    {
        D#\=32
    },
    [D],
    num_from_file(Ds).

num_from_file([])-->
    [].

space -->
    [32].

% DCG to parse the list to data
node(Sum, Value) -->
    num(ChildNum),
    num(MetadataNum),
    nodeList(ChildNum, ChildSum, ChildValues),
    mList(MetadataNum, MetadataValues),
    { 
        ChildNum #>0,
        sum_list(MetadataValues, MetadataSum),
        Sum#=ChildSum+MetadataSum,
        calc_node_value(ChildValues, MetadataValues, Value)
    }.

% This is not the cause of non-termination - a false at the end causes
% program to terminate
node(MetadataSum, MetadataSum) -->
    num(0),
    num(MetadataNum),
    mList(MetadataNum, Vals),
    {
        sum_list(Vals, MetadataSum)
    }.

% A false at the beginning does not cause program to terminate
nodeList(0, 0, []) -->
    [].
% The recursive call to nodeList is non-terminating
nodeList(N, Sum, [NodeValue|Values]) -->
    { 
        N #>0,
        N1#=N-1,
        Sum#=NodeSum+ListSum
    },
    node(NodeSum, NodeValue),
    nodeList(N1, ListSum, Values).

mList(0,[]) -->
    [].

mList(N, [E|NodeReferences]) -->
    { N#>0,
      N1#=N-1
    },
    num(E),
    mList(N1, NodeReferences).

num(N) -->
    [N].

calc_node_value(Values, References, Value):-
    maplist(my_nth(Values), References, L1),
    sum_list(L1, Value).

my_nth(List, Index, 0):-
    \+nth1(Index, List, _).

my_nth(List, Index, E):-
    nth1(Index, List, E).

sample_list([3,2,2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2,1, 1, 2,0,1,33,0,0,2,1]).