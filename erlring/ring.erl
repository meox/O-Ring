-module(ring).
-export([main/1, initialize/1]).


main([Nx, Mx]) ->
    N = list_to_integer(atom_to_list(Nx)),
    M = list_to_integer(atom_to_list(Mx)),
    start(N,M).


start(N, M) ->
    StartRing = erlang:monotonic_time(),
    Head = prepareFirst(N),
    buildRing(N),
    Head ! { rollcall, N, self() },
    receive 
	allNodesUp -> StopRing = erlang:monotonic_time()
    end,
    Start = erlang:monotonic_time(),
    Head ! N*M,
    receive
	done -> Stop = erlang:monotonic_time()
    end,
    RingCreation = erlang:convert_time_unit(StopRing-StartRing, native, millisecond),
    ElapsedTime = erlang:convert_time_unit(Stop-Start, native, millisecond),
    io:format("~p ~p ~p ~p~n", [RingCreation, ElapsedTime, N, M]).


prepareFirst(N) ->
    Head = spawn(?MODULE, initialize, [self()]),
    receive
	{Head, ok} -> Head ! { forkNewProcess, N-1, Head, self()}
    end,
    Head.


buildRing(N) -> setupRing(N, 1).


setupRing(N, N) -> void;
setupRing(N, K) ->
    receive 
	ready -> setupRing(N, K+1)
    end.


loop(Next, Parent) ->
    receive
        { rollcall, 0, Parent } -> 
	    Parent ! allNodesUp,
	    loop(Next, Parent);
        { rollcall, N, Parent } -> 
	    Next ! { rollcall, N-1, Parent },
	    loop(Next, Parent);
	0 -> Parent ! done;
	N -> Next ! (N-1),
	     loop(Next, Parent)
    end.


prepare(0, OpenHead, Parent) ->
    Parent ! ready,
    loop(OpenHead, Parent);

prepare(N, OpenHead, Parent) ->
    Next = spawn(?MODULE, initialize, [self()]),
    receive
	{Next, ok} -> Next ! { forkNewProcess, N-1, OpenHead, Parent}
    end,
    Parent ! ready,
    loop(Next, Parent).


initialize(Previous) ->
    Previous ! {self(), ok},
    receive
	{ forkNewProcess, N, OpenHead, Parent } ->
	    prepare(N, OpenHead, Parent)
    end.
