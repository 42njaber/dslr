
:- module( common, [
	mean/2,
	std/2,
	percentile/3,
	shorten/2
]).

mean(Ns, Mean):- length(Ns, C),
	sum_list(Ns, Sum), Mean is Sum / C.
std(Ns, Std):- length(Ns, C),
	mean(Ns, Mean),
	maplist( {Mean}/[N,SE]>>(SE is (N - Mean) ** 2), Ns, SquaredErrors ),
	sum_list(SquaredErrors, SumSquaredErrors), Std is (SumSquaredErrors / C) ** 0.5.
percentile(P, Ns, Percentile):- length(Ns, C),
	msort(Ns, Sorted),
	PercentileN is (C - 1) * P // 100, nth0(PercentileN, Sorted, Percentile).

shorten(Name, Max):- shorthand(Name, Max, Short), atom_length(Short, Len), Len =< Max, write(Short).

shorthand(Name, _, Name).
shorthand(Name, _, Short):-
	atomic_list_concat(Words, ' ', Name), Words \= [_],
	maplist( [A,I]>>(I = A ; sub_atom(A,0,1,_,I)), Words, Initials ),
	atomic_list_concat(Initials, '.', Short).
shorthand(Name, Max, Short):- Len is Max - 2, sub_atom(Name, 0, Len, _, Pre), atomic_list_concat([Pre, '.'], Short).

:- use_module(library(pce)).

:- pce_begin_class(marker, pixmap).

initialise(W, Color:colour) :->
	send_super(W, initialise('marker.xbm', Color, @default)),
	send(W, mask, bitmap('marker.xbm')).

:- pce_end_class.
