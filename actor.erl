-module(actor).
-compile(export_all).

start(From, SelfId, gossip) ->  
    worker(gossip, From, SelfId, 10).

start(From, SelfId, pushsum, {S, W}) -> worker(From, SelfId, pushsum, {S, W}).

worker(pushsum, From, SelfId, {S, W}) -> worker(pushsum, From, SelfId, {S, W}, [0, 0, 0]);
    
worker(gossip, From, SelfId, K) ->
    receive 
        {From, gossip, Message} ->
            io:format("qwertyuiop~n"),
            KK = gossip(From, Message, SelfId, K)
    end,
    worker(gossip, From, SelfId, KK).

worker(pushsum, From, SelfId, {S, W}, [prev1, prev2, prev3]) -> 
    receive
        {From, {pushsum, Message, _S, _W}} ->
            io:format("~p ~p ~p ~p~n", [Message, S, W]),
            {S, W, [prev1, prev2, prev3]} = pushsum(Message, S, W, _S, _W);
        Other ->
            io:format("Error, received ~w.~n", [Other])
    end,
    worker(From, pushsum, {S, W}, [prev1, prev2, prev3]).

% gossip(From, Message, SelfId, 0) -> 0;

gossip(From, Message, SelfId, K) -> 
    io:format("asdfgh"),
    From ! {self(), gossip, SelfId, Message},
    if  
        K-1 == 0 -> 
            From ! {SelfId, terminate};
            K-1;
        true -> K-1
    end.    

pushsum(Message, S, W, _S, _W) -> 
    io:format("Push-Sum~n"),
    {S, W, [prev1, prev2, prev3]}.
