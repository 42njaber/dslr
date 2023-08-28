
:- use_module(library(pce)).
:- use_module(library(plot/axis)).
:- use_module(library(plot/barchart)).

:- use_module(common).
:- use_module(library(clpfd), [transpose/2]).

histogram(DataFile, _):-
	catch_with_backtrace(
		csv_read_file(DataFile, Data, [convert(true)]),
		E, (print_message(error, E), false)
	),
	maplist( [R, D]>>(R =.. [_|D]), Data, Rows),
	transpose( Rows, Cols ),
	ignore(get(histogram(Cols), confirm, _)).

:- pce_begin_class(histogram, dialog).

variable(data, prolog_term, get).

initialise(W, [_Idx,Houses|Data]:prolog_term) :->

	send_super(W, initialise('Histogram')),

	exclude( [[_|Col]]>>include(number, Col, []), Data, Features ),

	maplist( nth0(0), Features, Parameters ),
	send(W, slot, data, [Houses|Features]),

	send(W, append, new(@parameter, menu(parameter, choice, message(W, plot, W?data, @parameter?selection)))),
	send_list(@parameter, append, Parameters),
	send(@parameter, layout, horizontal),
	send(@parameter, columns, 4),

	send(W, append, new(@hist, dialog_group(histogram))),
	send(W, plot, [Houses|Features], @parameter?selection),

	send(W, open)
	.

plot(W, D:prolog_term, Parameter) :->
	D = [[_|DataHouses]|Features],
	member( [Parameter|DataNumbers], Features ),
	transpose([DataHouses,DataNumbers], Data),
	include( [[H,N]]>>number(N), Data, Fields ),
	transpose(Fields, [_,Numbers]),
	percentile(000, Numbers, Min), percentile(100, Numbers, Max),
	findall( [X0,X1]-B, (
		between(0, 24, Step),
		X0 is Min + (Max - Min) * Step / 25,
		X1 is Min + (Max - Min) * (Step + 1) / 25,
		include( {X0,X1}/[[_,N]]>>(X0 =< N, N < X1), Fields, InRange ),
		maplist(
			{InRange}/[C,House]>>( include( [[House,_]]>>true, InRange, Selected ), length(Selected, C) ),
			B, ['Gryffindor','Hufflepuff','Slytherin','Ravenclaw']
		)
	), Vals ),
	pairs_values(Vals, Bars),
	append(Bars, All), max_list(All, Height),
	send(@hist, clear),
	send(@hist, append, new(Chart, bar_chart(vertical, 0, Height, 300, 25))),
	foreach( member( [X0,X1]-[G, H, S, R], Vals ),
		send(Chart, append, bar_group( X0-X1, bar(g, G, red), bar(h, H, orange), bar(s, S, green), bar(r, R, purple) ) )
	),
	send(@hist, layout_dialog).

:- pce_end_class.

%% Main

:- use_module(library(main)).

:- dynamic opt_type/3, opt_help/2, opt_meta/2.

opt_help(help(usage),' [options] <datafile>').

main(Argv):-
	argv_options(Argv, Args, Opts),
	main(Args, Opts).
main([DataFile], Opts):- \+ member(help(true),Opts), 
	exists_file(DataFile), !,
	histogram(DataFile, Opts).
main(_, _):- argv_usage(informational).

%% Debug

:- use_module(library(prolog_stack)).
:- initialization(main_, main).

main_:- catch_with_backtrace(main, Err, print_message(error, Err)).

:- det(main/1).
