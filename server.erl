-module(server).
-compile(export_all).

spawnProcess(Algorithm, Topology, NumActors) -> spawn(server, start, [Algorithm, Topology, NumActors]).

start(Algorithm, Topology, NumActors) -> 
    statistics(runtime),
    statistics(wall_clock),
    Grid = getGridMap(Algorithm, Topology, NumActors, maps:new()),
    io:format("~p~n", [Grid]),
    start(Algorithm, Topology, NumActors, Grid, 0, []).

start(Algorithm, Topology, NumActors, Grid, NodesCompleted, TerminationLog) -> 
    if
        NumActors == NodesCompleted -> terminate(Grid, TerminationLog);
        true -> A = 1
    end,
    receive 
        {From, gossip, NodeId, Message} -> 
            processMessage(From, gossip, NodeId, Message, Grid, Topology);
        {From, pushsum, Topology, Message, {S, W}} -> processMessage(From, pushsum, Topology, Message, {S, W});
        {From, terminate} -> start(Algorithm, Topology, NumActors, Grid, NodesCompleted+1, TerminationLog ++ [{NodesCompleted+1, getTime()}])
    end,
    start(Algorithm, Topology, NumActors, Grid, NodesCompleted, TerminationLog).

getTime() -> 
    {_, Time1} = statistics(runtime),
    Time1.

getGridMap(Algorithm, line, 0, GridMap) -> GridMap;

getGridMap(Algorithm, line, NumActors, GridMap) -> 
    Pid = spawn(actor, start, [self(), NumActors, Algorithm]),
    GridMap1 = GridMap#{NumActors => Pid}, 
    getGridMap(Algorithm, line, NumActors-1, GridMap1);

getGridMap(Algorithm, fullnetwork, NumActors, GridMap) -> getGridMap(Algorithm, line, NumActors, GridMap);

getGridMap(Algorithm, grid3d, NumActors, GridMap) -> 
    N = round(math:pow(NumActors, 1/3)),
    get3dGrid(N, N, N, GridMap, Algorithm).

processMessage(From, gossip, NodeId, Message, Grid, Topology) -> 
    NeighborIds = getNeighbors(NodeId, Grid, Topology),
    sendToNeighbors(NeighborIds, Message, Grid).

sendToNeighbors([], Message, Grid) -> 
    io:format("sent to all neighbors~n");

sendToNeighbors([X|Y], Message, Grid) -> 
    Neighbor = maps:get(X, Grid),
    Neighbor ! {self(), gossip, Message},
    sendToNeighbors(Y, Message, Grid).

processMessage(From, pushsum, Topology, Message, {S, W}) -> io:format("").

getNeighbors(NodeId, Grid, line) -> 
    List = [] ++ validIndex(NodeId-1, Grid) ++ validIndex(NodeId+1, Grid);

getNeighbors(NodeId, Grid, fullnetwork) -> getRandom(maps:size(Grid), NodeId);

getNeighbors({M, N, P}, Grid, grid3d) ->
    List = validIndex({M-1, N-1, P}, Grid) ++ validIndex({M-1, N, P}, Grid) ++ validIndex({M, N-1, P}, Grid),
    List1 = List ++ validIndex({M+1, N+1, P}, Grid) ++ validIndex({M+1, N, P}, Grid) ++ validIndex({M, N+1, P}, Grid),
    List2 = List1 ++ validIndex({M+1, N-1, P}, Grid) ++ validIndex({M-1, N+1, P}, Grid),
    RandomPlaneIndex = getRandom(math:pow(maps:size(Grid), 1/3), P),
    List2 ++ [{M,N,RandomPlaneIndex}];


getNeighbors({M, N}, Grid, grid2d) ->  
    List = validIndex({M-1, N-1}, Grid) ++ validIndex({M-1, N}, Grid) ++ validIndex({M, N-1}, Grid) ++ validIndex({M+1, N+1}, Grid),
    List ++ validIndex({M+1, N-1}, Grid) ++ validIndex({M-1, N+1}, Grid) ++ validIndex({M+1, N}, Grid) ++ validIndex({M, N+1}, Grid).

validIndex(NodeId, Grid) -> 
    IsValid = maps:is_key(NodeId, Grid),
    if
        IsValid == true -> [NodeId];
        true -> []
    end.

get2dGrid(0, N, GridMap, Algorithm) -> GridMap;
get2dGrid(M, 0, GridMap, Algorithm) -> GridMap;
get2dGrid(M, N, GridMap, Algorithm) ->
    Index = prepend(M, prepend(N, {})),
    Pid = qwert,spawn(actor, start, [self(), Index, Algorithm]),
    GridMap1 = GridMap#{Index => Pid},
    GridMap2 = get2dGrid(M-1, N, GridMap1, Algorithm),
    get2dGrid(M, N-1, GridMap2, Algorithm).

get3dGrid(0, N, P, GridMap, Algorithm) -> GridMap;
get3dGrid(M, 0, P, GridMap, Algorithm) -> GridMap;
get3dGrid(M, N, 0, GridMap, Algorithm) -> GridMap;
get3dGrid(M, N, P, GridMap, Algorithm) ->
    Index = prepend(M, prepend(N, prepend(P, {}))),
    Pid = qwert,spawn(actor, start, [self(), Index, Algorithm]),
    GridMap1 = GridMap#{Index => Pid},
    GridMap2 = get3dGrid(M, N, P-1, GridMap1, Algorithm),
    GridMap3 = get3dGrid(M, N-1, P, GridMap2, Algorithm),
    get3dGrid(M-1, N, P, GridMap3, Algorithm).

prepend(X, {}) -> {X};
prepend(X, {A}) -> {X, A};
prepend(X, {A, B}) -> {X, A, B}.

getRandom(N, CurrNodeId) ->
    Q = rand:uniform(N),
    if
        Q == CurrNodeId -> getRandom(N, CurrNodeId);
        true -> Q
    end.

terminate(Grid, TerminationLog) -> 
    List = maps:to_list(Grid),
    terminate(List),
    io:format("~p",[TerminationLog]),
    exit(self(), kill).

terminate([]) -> io:format("Terminate Successfully!");
terminate([X|Y]) -> 
    {_, Pid} = X,
    exit(Pid, kill),
    terminate(Y).