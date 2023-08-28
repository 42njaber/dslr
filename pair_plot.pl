
:- use_module(library(pce)).
:- use_module(library(plot/axis)).
:- use_module(library(plot/plotter)).
:- use_module(library(plot/barchart)).

:- use_module(common).
:- use_module(library(clpfd), [transpose/2]).

scatter(DataFile, _):-
	catch_with_backtrace(
		csv_read_file(DataFile, Data, [convert(true)]),
		E, (print_message(error, E), false)
	),
	maplist( [R, D]>>(R =.. [_|D]), Data, Rows),
	transpose( Rows, Cols ),
	ignore(get(plot_matrix(Cols), confirm, _)).

:- pce_begin_class(plot_matrix, dialog).

initialise(W, [_Idx,Houses|Data]:prolog_term) :->

	send_super(W, initialise('Pair plot')),

	exclude( [[_|Col]]>>include(number, Col, []), Data, Features ),

	send(W, append, new(@plot, device)),
	send(W, plot, [Houses|Features]),

	send(W, open).

plot(_, D:prolog_term) :->
	D = [[_|Houses]|Features],
	send(@plot, clear),

	send(@plot, layout_manager, new(Matrix, table)),
	send(Matrix, border, 1),
	send(Matrix, frame, box),
	send(Matrix, rules, all),
	foreach(
		( member( [F|_], [[''|_]|Features] ), _ = [T, C] ),
		(
			format(string(T), "~@", shorten(F, 10)),
			new(C, table_cell(text(T, center, font(mono, bold, 11)))),
			send(C, halign, center),
			send(C, valign, bottom),
			send(Matrix, append, C )
		)
	),
	foreach(
		( member( [F1|D1], Features ), _ = [T, C] ),
		(
			format(string(T), "~@", shorten(F1, 10)),
			new(C, table_cell(text(T, center, font(mono, bold, 11)))),
			send(C, halign, right),
			send(C, valign, center),
			send(Matrix, next_row),
			send(Matrix, append, C ),
			foreach(
				member( [F2|D2], Features ),
				( F1 = F2 -> send(Matrix, append, matrix_bars(Houses, D1)) ; send(Matrix, append, matrix_pair(Houses, D1, D2)) )
			)
		)
	).

:- pce_end_class.

:- pce_begin_class(matrix_bars, bar_chart).

initialise(P, Houses:prolog_term, RawFeature:prolog_term):->
	transpose([Houses,RawFeature], Raw),
	include( [[H,N]]>>number(N), Raw, Data ),
	transpose(Data, [_,Numbers]),
	percentile(000, Numbers, Min), percentile(100, Numbers, Max),
	findall( [X0,X1]-B, (
		between(0, 9, Step),
		X0 is Min + (Max - Min) * Step / 10,
		X1 is Min + (Max - Min) * (Step + 1) / 10,
		include( {X0,X1}/[[_,N]]>>(X0 =< N, N < X1), Data, InRange ),
		maplist(
			{InRange}/[C,House]>>( include( [[House,_]]>>true, InRange, Selected ), length(Selected, C) ),
			B, ['', 'Gryffindor','Hufflepuff','Slytherin','Ravenclaw']
		)
	), Vals ),
	pairs_values(Vals, Bars),
	append(Bars, All), max_list(All, Height),
	send_super(P, initialise, vertical, 0, Height + 1, 70, 10, 6.5, 1),
	foreach( member( [X0,X1]-[N, G, H, S, R], Vals ),
		send(P, append, bar_group( ' ', bar(n, N, black), bar(g, G, red), bar(h, H, orange), bar(s, S, green), bar(r, R, purple) ) )
	),
	send(P?graphicals, for_all, if( not(message(@arg1, instance_of, bar_group)), message(@arg1, displayed, @off) ) )
	.

:- pce_end_class.

:- pce_begin_class(matrix_pair, bitmap).

initialise(I, Houses:prolog_term, Raw1:prolog_term, Raw2:prolog_term):->
	new(ITmp, pixmap(@nil, @default, grey, 500, 500)),
	new(P, plotter),
	transpose([Houses,Raw1,Raw2], Raw),
	include( [[_,N1,N2]]>>(number(N1), number(N2)), Raw, Data ),
	transpose(Data, [_,D1,D2]),
	min_member(Min1, D1), max_member(Max1, D1),
	min_member(Min2, D2), max_member(Max2, D2),
	send(P, axis, plot_axis(x, Min1, Max1, @default, 500) ),
	send(P, axis, plot_axis(y, Min2, Max2, @default, -500) ),
	foreach( ( member(House-Color, [''-black, 'Gryffindor'-red,'Hufflepuff'-orange,'Slytherin'-green,'Ravenclaw'-purple]), _ = [Graph] ),
		(
			send(P, graph, new(Graph, plot_graph(points_only, marker(colour(Color))))),
			foreach( member([House, X, Y], Data), send(Graph, append, X, Y) ),
			send(ITmp, draw_in, Graph)
		)
	),
	get( ITmp, scale, size(70, 70), IScale ),
	send_super(I, initialise, IScale, @on).

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
