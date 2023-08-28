
:- use_module(common).
:- use_module(formulas).
:- use_module(features).
:- use_module(library(clpfd), [transpose/2]).

learning_rate(0.2).
max_iter(10000).
min_delta(0.0000001).
target_delta_step(0.001).

train(DataFile, _Opts):-
	catch_with_backtrace(
		csv_read_file(DataFile, Raw, [convert(true)]),
		E, (print_message(error, E), false)
	),
	filter(Raw, Data),
	normalize(Data, Normalized, Norme),
	writeln(Norme)
	%format('Training started~n'),
	%initialize_thetas(Normalized,Thetas0),
	%learning_rate(R),
	%max_iter(I),
	%( member(auto_rate(true), Opts) -> Rate = auto(R,1) ; Rate = R ),
	%train_iter(I, Rate, Normalized, Thetas0, Thetas),
	%format('Training complete~n'),
	%save_thetas( Thetas, Factors )
	.

filter(DataCsv, Filtered):-
	maplist( [R, C]>>(R =.. [_|C]), DataCsv, Data ),
	transpose(Data, [_,House|F1]),
	include( [[Head|Col]]>>(include(number, Col, [_|_]), train_feature(Head)), F1, F2 ),
	transpose([House|F2], [_|Rows]),
	include( [[_|Fs]]>>maplist(number, Fs), Rows, Filtered ).

normalize(Data, [Houses|Normalized], Norme):-
	transpose(Data, [Houses|Features]),
	maplist( [F, N, range(Min, Max)]>>(
		min_member(Min, F), max_member(Max, F),
		maplist( {Min,Max}/[X1, X2]>>(X2 is (X1 - Min) / (Max - Min)), F, N )
	), Features, NormalizedFeatures, Norme ),
	transpose(NormalizedFeatures, Normalized).

%train_iter(I, Rate, ThetasIn, ThetasOut).

%% Main

:- use_module(library(main)).

:- dynamic opt_type/3, opt_help/2, opt_meta/2.

opt_help(help(usage),' [options] <datafile>').

main(Argv):-
	argv_options(Argv, Args, Opts),
	main(Args, Opts).
main([DataFile], Opts):- \+ member(help(true),Opts), 
	exists_file(DataFile), !,
	train(DataFile, Opts).
main(_, _):- argv_usage(informational).

%% Debug

:- use_module(library(prolog_stack)).
:- initialization(main_, main).

main_:- catch_with_backtrace(main, Err, print_message(error, Err)).

:- det(main/1).
:- det(train/2).
