
:- use_module(library(pce)).
:- use_module(library(plot/axis)).
:- use_module(library(plot/plotter)).

:- use_module(common).
:- use_module(library(clpfd), [transpose/2]).

scatter(DataFile, _):-
	catch_with_backtrace(
		csv_read_file(DataFile, Data, [convert(true)]),
		E, (print_message(error, E), false)
	),
	maplist( [R, D]>>(R =.. [_|D]), Data, Rows),
	transpose( Rows, Cols ),
	ignore(get(scatter(Cols), confirm, _)).

:- pce_begin_class(scatter, dialog).

variable(data, prolog_term, get).

initialise(W, [_Idx,Houses|Data]:prolog_term) :->

	send_super(W, initialise('Histogram')),

	exclude( [[_|Col]]>>include(number, Col, []), Data, Features ),

	maplist( nth0(0), Features, Parameters ),
	send(W, slot, data, [Houses|Features]),

	send(W, append, new(@parameter, menu(parameters, choice, message(W, plot, W?data, @parameter?selection)))),
	send_list(@parameter, append, Parameters),
	send(@parameter, multiple_selection, true),
	send(@parameter, layout, horizontal),
	send(@parameter, columns, 4),

	send(W, append, new(@plot, dialog_group(plot))),

	send(W, open)
	.

plot(W, D:prolog_term, ParamsChain ) :->
	chain_list(ParamsChain, [Parameter1,Parameter2]),
	D = [_|Features],
	member( [Parameter1|Raw1], Features ),
	member( [Parameter2|Raw2], Features ),

	transpose([Raw1,Raw2], Raw),
	include( [[N1, N2]]>>(number(N1), number(N2)), Raw, Data ),
	transpose(Data, [Data1,Data2]),
	min_member(Min1, Data1), max_member(Max1, Data1), 
	min_member(Min2, Data2), max_member(Max2, Data2), 

	send(@plot, clear),
	send(@plot, append, new(P, plotter)),
	send(P, axis, plot_axis(x, Min1, Max1, @default, 400) ),
	send(P, axis, plot_axis(y, Min2, Max2, @default, 400) ),

	send(P, graph, new(PointsGraph, plot_graph(points_only, marker(black)))),

	foreach( member( [X,Y], Data ),
		send(PointsGraph, append, X, Y )
	),
	send(@plot, layout_dialog),
	send(W, '_compute_desired_size').

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
	scatter(DataFile, Opts).
main(_, _):- argv_usage(informational).

%% Debug

:- use_module(library(prolog_stack)).
:- initialization(main_, main).

main_:- catch_with_backtrace(main, Err, print_message(error, Err)).

:- det(main/1).
