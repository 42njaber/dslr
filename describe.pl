
:- use_module(common).
:- use_module(library(clpfd), [transpose/2]).

describe(DataFile, _):-
	catch_with_backtrace(
		csv_read_file(DataFile, [Head|Rows], [convert(true)]),
		E, (print_message(error, E), false)
	),
	description(Rows, Descs),
	Head =.. [_,_|Headers],
	print_descs(Headers, Descs), !.

description(Rows, Descs):-
	maplist( [R,D]>>(R =.. [_,_|D]), Rows, Data ),
	transpose(Data, Cols),
	maplist(column_desc, Cols, Descs).

column_desc(Col, desc(Count, Mean, Std, Min, Percent25, Percent50, Percent75, Max)):-
	include(number, Col, Numbers), Numbers \= [], !,
	length(Numbers, Count),
	mean(Numbers, Mean),
	std(Numbers, Std),
	percentile(000, Numbers, Min),
	percentile(025, Numbers, Percent25),
	percentile(050, Numbers, Percent50),
	percentile(075, Numbers, Percent75),
	percentile(100, Numbers, Max).
column_desc(_, desc(none)).

print_descs(Headers, Descs):-
	format( '~|~t~10+' ),
	foreach( ( nth1(N, Descs, desc(_,_,_,_,_,_,_,_) ), nth1(N, Headers, H) ),
		format( '~|~t~@~11+', [shorten(H, 10)] )
	), nl,
	foreach( member(Name-V, ['Count'-C,'Mean'-M,'Std'-S,'Min'-Min,'25%'-P25,'50%'-P50,'75%'-P75,'Max'-Max]), (
		format( '~|~w~t~10+', [Name] ),
		foreach( ( nth1(N, Descs, desc(C,M,S,Min,P25,P50,P75,Max)) ),
			format( '~|~t~7g~11+', [V] )
		), nl
	) ).

%% Main

:- use_module(library(main)).

:- dynamic opt_type/3, opt_help/2, opt_meta/2.

opt_help(help(usage),' [options] <datafile>').

main(Argv):-
	argv_options(Argv, Args, Opts),
	main(Args, Opts).
main([DataFile], Opts):- \+ member(help(true),Opts), 
	exists_file(DataFile), !,
	describe(DataFile, Opts).
main(_, _):- argv_usage(informational).

%% Debug

:- use_module(library(prolog_stack)).
:- initialization(main_, main).

main_:- catch_with_backtrace(main, Err, print_message(error, Err)).

:- det(main/1).
