-module(conc).
-export([recv/0]).

recv() -> receive
    Msg -> io:format("Recv got: ~p~n", [Msg])
end.