-module(client).
-export([serve/1]).

serve(Bank) ->
    Bank ! {client, add, self()},
    serve(Bank, []).

serve(Bank, CurrentTransactions) ->
    receive
        {transfer, new, To} ->
            Bank ! {client, transfer, self(), To},
            serve(Bank, [To|CurrentTransactions]);
        {transfer, remaining, Asker} ->
            Asker ! {client, remain, self(), length(CurrentTransactions)},
            serve(Bank, CurrentTransactions);
        {transfer, error, To} ->
            case length(lists:filter(fun(E) -> E == To end, CurrentTransactions)) of
                0 -> ok;
                1 -> Bank ! {client, transfer, self(), To}
            end,
            serve(Bank, CurrentTransactions);
        {transfer, ok, To} ->
            serve(Bank, CurrentTransactions -- [To]);
        M ->
            io:format("Client ~p recieved: ~p~n", [self(), M]),
            serve(Bank, CurrentTransactions)
    end.
