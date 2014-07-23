%%% ====================================================================
%%% This software is copyright (c) 2006-2014, ProcessOne.
%%%
%%% @copyright 2006-2014 ProcessOne
%%% @author Geoff Cant <geoff.cant@process-one.net>
%%%   [http://www.process-one.net/]
%%% @author Christophe Romain <christophe.romain@process-one.net>,
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @doc The module <strong>{@module}</strong> manages operations on a
%%% cluster of erlang nodes running CEAN. It handles package
%%% syncronisation/installation/upgrade/removal and management of
%%% cluster nodes.
%%%
%%% @reference See <a href="http://cean.process-one.net">CEAN Web Site</a>
%%% for detailed CEAN description.

-module(cean_cluster).

-export([start/0,
         stop/0,
         start_nodes/0,
         start_nodes/1,
         start_node/1,
         stop_nodes/0,
         stop_nodes/1,
         stop_node/1,
         ping_node/1]).

-export([install/1,
         install/2,
         uninstall/1,
         uninstall/2,
         sync/0,
         sync/1,
         status/0,
         status/1,
         is_synchronised/0,
         is_synchronised/1]).

-export([is_cean_node/0,
         is_cean_node/1,
         cean_nodes/0,
         cean_nodes/1]).

-export([call/0,
         help/0,
         help/1]).

-record(diff, {same=[],
               missing=[],
               extra=[]}).

%% Perform Fun(Node) -> [{Node, Result}] on a list of nodes.
%% TODO: run actions in parallel.
action(Action) ->
    action(Action, all).
action(Action, Nodes) ->
    lists:map(fun (Node) -> {Node, Action(Node)} end,
              cean_nodes(Nodes)).

%% Function adapter to allow calling cean module operations remotely.
%% Used with action to perform the same cean operations on
%% lists of nodes.
cean_call(Function, Arguments) ->
    fun (Node) -> catch rpc:call(Node, cean, Function, Arguments) end.

%% start/stop cluster nodes

start() ->
    Known = cean_nodes(),
    Nodes = lists:filter(fun (Node) ->
                not lists:member(Node, Known)
            end, load_state()),
    start(Nodes, Known).
start([Node|Tail], Known) ->
    case ping_node(Node) of
        pong ->
            NewKnown = cean_nodes(),
            Nodes = lists:filter(fun (N) ->
                        not lists:member(N, NewKnown)
                    end, Tail),
            start(Nodes, NewKnown);
        _ ->
            case start_node(Node) of
                {ok, _} -> start(Tail, [Node|Known]);
                _ -> start(Tail, Known)
            end
    end;
start([], Known) ->
    case cean_nodes() of
        [] -> error;
        Known -> complete;
        _ -> partial
    end.

start_nodes() ->
    action(fun start_node/1).
start_nodes(Nodes) when is_list(Nodes) ->
    action(fun start_node/1, Nodes).
start_node(Node) when is_list(Node) ->
    start_node(list_to_atom(Node));
start_node(Node) when Node =:= node() ->
    ok;
start_node(Node) when is_atom(Node) ->
    case slave:start(node_host(Node)) of
        {OK, N} when OK =:= ok;
                     OK =:= already_running ->
            (cean_call(set_server, [cean:get_server()]))(N),
            {ok, N};
        Error ->
            {error, {"Couldn't start slave node", Error}}
    end.

stop() ->
    Nodes = cean_nodes(),
    save_state(Nodes),
    stop_nodes(Nodes).

stop_nodes() ->
    action(fun stop_node/1).
stop_nodes(Nodes) when is_list(Nodes) ->
    action(fun stop_node/1, Nodes).
stop_node(Node) when is_list(Node) ->
    stop_node(list_to_atom(Node));
stop_node(Node) when Node =:= node() ->
    cean:stop();
stop_node(Node) when is_atom(Node) ->
    slave:stop(node_host(Node)).

ping_node(Node) when is_list(Node) ->
    ping_node(list_to_atom(Node));
ping_node(Node) when is_atom(Node) ->
    case is_cean_node(Node) of
        true -> net_adm:ping(Node);
        false -> badarg
    end.

%% packages actions

install(Package) ->
    action(cean_call(install, [Package])).
install(Package, Nodes) ->
    action(cean_call(install, [Package]), Nodes).

uninstall(Package) ->
    action(cean_call(uninstall, [Package])).
uninstall(Package, Nodes) ->
    action(cean_call(uninstall, [Package]), Nodes).

sync() ->
    action(fun sync/1).
sync(Nodes) when is_list(Nodes) ->
    action(fun sync/1, Nodes);
sync(Node) when is_atom(Node) ->
    {Missing, Extra} = case compare(node(), Node) of
        #diff{missing=M, extra=E} -> {M, E};
        _ -> {[], []}
    end,
    [{install, P, (cean_call(install, [P]))(Node)} || P <- Missing] ++
    [{uninstall, P, (cean_call(uninstall, [P]))(Node)} || P <- Extra].

sync_from(Node) when is_atom(Node) ->
    case catch rpc:call(Node, cean_cluster, sync, []) of
        Res when is_list(Res) -> Res;
        Error -> Error
    end.

status() ->
    action(fun status/1).
status(Nodes) when is_list(Nodes) ->
    action(fun status/1, Nodes);
status(Node) when Node =:= node() ->
    master;
status(Node) when is_atom(Node) ->
    case compare(node(), Node) of
        #diff{missing=[],extra=[]} ->
            synchronised;
        #diff{missing=M,extra=E} ->
            [T || T = {_, List} <- [{missing, M}, {extra, E}],
                  List =/= []]
    end.

is_synchronised() ->
    is_synchronised(remote).
is_synchronised(Nodes) ->
    lists:all(fun (Node) -> status(Node) =:= synchronised end,
              cean_nodes(Nodes)).

installed(Node) ->
    case (cean_call(installed, []))(Node) of
        Installed when is_list(Installed) -> Installed;
        _ -> []
    end.
compare(Node, Node) ->
    #diff{};
compare(NodeA, NodeB)
 when is_atom(NodeA), is_atom(NodeB) ->
    compare(installed(NodeA), installed(NodeB));
compare(PackageListA, PackageListB)
 when is_list(PackageListA), is_list(PackageListB) ->
    PackageSetA = sets:from_list(PackageListA),
    PackageSetB = sets:from_list(PackageListB),
    Same = sets:intersection(PackageSetA, PackageSetB),
    Missing = sets:subtract(PackageSetA, PackageSetB),
    Extra = sets:subtract(PackageSetB, PackageSetA),
    #diff{same=sets:to_list(Same),
          missing=sets:to_list(Missing),
          extra=sets:to_list(Extra)}.

%% cean nodes resolver

is_cean_node() ->
    case cean:get_server() of
        false -> false;
        _ -> true
    end.
is_cean_node(Node) ->
    case catch rpc:call(Node, cean_cluster, is_cean_node, []) of
        true -> true;
        _ -> false
    end.

cean_nodes() ->
    cean_nodes(all).
cean_nodes(all) ->
    cean_nodes([node() | nodes()]);
cean_nodes(remote) ->
    cean_nodes(nodes());
cean_nodes(Node) when is_atom(Node) ->
    cean_nodes([Node]);
cean_nodes(Nodes) when is_list(Nodes) ->
    [Node || Node <- Nodes, is_cean_node(Node)];
cean_nodes(_) ->
    [].

node_host(Node) when is_atom(Node) ->
    case string:tokens(atom_to_list(Node), "@") of
        [_Name, Host] -> list_to_atom(Host);
        [Host] -> list_to_atom(Host)
    end.

% internal status functions

save_state(State) ->
    File = filename:join([code:privdir(cean), "cluster"]),
    file:write_file(File, term_to_binary(State)).
load_state() ->
    File = filename:join([code:privdir(cean), "cluster"]),
    case file:read_file(File) of
        {ok, Binary} -> binary_to_term(Binary);
        _ -> []
    end.

%% bash call entry

call() ->
    case init:get_plain_arguments() of
    [Node, Cmd | Args] ->
        F = list_to_atom(Cmd),
        N = list_to_atom(Node),
        R = rpc:call(N, ?MODULE, F, Args, 10000),
        io:format("~p~n", [R]);
    Bad ->
        io:format("{badarg, ~p}~n", [Bad])
    end,
    erlang:halt().

%% inline help

help() ->
    Exported = lists:usort( [Function || {Function, _Arity} <- ?MODULE:module_info(exports)] ),
    io:fwrite( "The functions are: ~p~n", [Exported] ),
    io:fwrite( "Do help(<function>) for more help on that function.~n" ).
help( stop ) ->
    io:fwrite( "~w:stop( ). => ok~n", [?MODULE] ),
    io:fwrite( "stop the cluster.~n" );
help( start ) ->
    io:fwrite( "~w:start( ). => ok~n", [?MODULE] ),
    io:fwrite( "start the cluster.~n" );
help( stop_nodes ) ->
    io:fwrite( "~w:stop_nodes( ). => ok~n", [?MODULE] ),
    io:fwrite( "stop all cean nodes.~n" ),
    io:fwrite( "~w:stop_nodes( Nodes ). => ok~n", [?MODULE] ),
    io:fwrite( "stop given cean nodes.~n" );
help( stop_node ) ->
    io:fwrite( "~w:stop_node( Node ). => ok~n", [?MODULE] ),
    io:fwrite( "stop given cean node.~n" );
help( start_nodes ) ->
    io:fwrite( "~w:start_nodes( ). => ok~n", [?MODULE] ),
    io:fwrite( "start all cean nodes.~n" ),
    io:fwrite( "~w:start_nodes( Nodes ). => ok~n", [?MODULE] ),
    io:fwrite( "start given cean nodes.~n" );
help( start_node ) ->
    io:fwrite( "~w:start_node( ). => ok~n", [?MODULE] ),
    io:fwrite( "start given cean node.~n" );
help( ping_node ) ->
    io:fwrite( "~w:ping_node( Node ). => pong|pang|badarg~n", [?MODULE] ),
    io:fwrite( "ping a cean node.~n" );
help( install ) ->
    io:fwrite( "~w:install( Package ). => ok~n", [?MODULE] ),
    io:fwrite( "install package on all nodes.~n" ),
    io:fwrite( "~w:install( Package, Nodes ). => ok~n", [?MODULE] ),
    io:fwrite( "install package on given nodes.~n" );
help( uninstall ) ->
    io:fwrite( "~w:uninstall( Package ). => ok~n", [?MODULE] ),
    io:fwrite( "uninstall pacakge on all nodes.~n" ),
    io:fwrite( "~w:uninstall( Package, Nodes ). => ok~n", [?MODULE] ),
    io:fwrite( "uninstall pacakge on given nodes.~n" );
help( sync ) ->
    io:fwrite( "~w:sync( ). => ok~n", [?MODULE] ),
    io:fwrite( "sync current node with all nodes.~n" ),
    io:fwrite( "~w:sync( Nodes ). => ok~n", [?MODULE] ),
    io:fwrite( "sync current node with given nodes.~n" ),
    io:fwrite( "~w:sync( Node ). => ok~n", [?MODULE] ),
    io:fwrite( "sync current node with given node.~n" );
help( status ) ->
    io:fwrite( "~w:status( ). => ok~n", [?MODULE] ),
    io:fwrite( "check if the cean packages are synchronised on all nodes,~n" ),
    io:fwrite( "report missing or additional packages if they differ.~n" ),
    io:fwrite( "~w:status( Nodes ). => ok~n", [?MODULE] ),
    io:fwrite( "check if the cean packages are synchronised on given nodes,~n" ),
    io:fwrite( "report missing or additional packages if they differ.~n" ),
    io:fwrite( "~w:status( Node ). => ok~n", [?MODULE] ),
    io:fwrite( "check if the cean packages are synchronised with given node,~n" ),
    io:fwrite( "report missing or additional packages if they differ.~n" );
help( is_synchronised ) ->
    io:fwrite( "~w:is_synchronized( ). => ok~n", [?MODULE] ),
    io:fwrite( "returns true if all nodes are syncronized.~n" ),
    io:fwrite( "~w:is_synchronized( Nodes ). => ok~n", [?MODULE] ),
    io:fwrite( "returns true if given nodes are syncronized.~n" );
help( is_cean_node ) ->
    io:fwrite( "~w:is_cean_node( ). => ok~n", [?MODULE] ),
    io:fwrite( "returns true if the current node is cean aware.~n" ),
    io:fwrite( "~w:is_cean_node( Node ). => ok~n", [?MODULE] ),
    io:fwrite( "returns true if given node is cean aware.~n" );
help( cean_nodes ) ->
    io:fwrite( "~w:cean_nodes( ). => []~n", [?MODULE] ),
    io:fwrite( "~w:cean_nodes( all ). => []~n", [?MODULE] ),
    io:fwrite( "returns a list of all cean nodes.~n" ),
    io:fwrite( "~w:cean_nodes( remote ). => []~n", [?MODULE] ),
    io:fwrite( "returns a list of cean nodes, except the current node.~n" ),
    io:fwrite( "~w:cean_nodes( Nodes ). => []~n", [?MODULE] ),
    io:fwrite( "filters given list of node to match cean nodes.~n" ),
    io:fwrite( "~w:cean_nodes( Node ). => []~n", [?MODULE] ),
    io:fwrite( "returns [Node] if cean node, else [].~n" );
help( Function ) ->
    case lists:member( Function, lists:usort([Exported || {Exported, _Arity} <- ?MODULE:module_info(exports)]) ) of
        true -> io:fwrite( "I have forgotten to write help for ~w~n", [Function] );
        false -> io:fwrite( "There is no ~w function~n", [Function] )
    end.
