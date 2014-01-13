-module(client).
-export([serve/1,transaction_runner/3]).

serve(Bank) ->
    Bank ! {client, add, self()},
    serve(Bank, []).

serve(Bank, CurrentTransactions) ->
    receive
        {transfer, new, To} ->
            serve(Bank, [transaction(self(), To, Bank)|CurrentTransactions]);
        {transfer, remaining, Asker} ->
            RemainingTransactions = lists:filter(fun(Pid) -> process_info(Pid) /= undefined end, CurrentTransactions),
            Asker ! {client, remain, self(), length(RemainingTransactions)},
            serve(Bank, RemainingTransactions);
        M ->
            io:format("Client ~p recieved: ~p~n", [self(), M]),
            serve(Bank, CurrentTransactions)
    end.

transaction(From, To, Bank) ->
    Tid = spawn(?MODULE, transaction_runner, [false, false, false]),
    Bank ! {client, transfer, From, To, Tid},
    Tid.

transaction_runner(From, To, Prepared) ->
    receive
        error ->
            case Prepared of
                false -> ok;
                true -> To ! rollback
            end,
            From ! {prepare, self()},
            transaction_runner(From, To, false);
        prepared -> walk(From, To, true);
        {from, NFrom} ->
            NFrom ! {prepare, self()},
            walk(NFrom, To, Prepared);
        {founded, NTo} -> walk(From, NTo, Prepared);
        ok -> ok;
        not_a_client -> not_a_client
    end.

walk(From, To, Prepared) ->
    case (From /= false) and (To /= false) and Prepared of
        false -> ok;
        true ->
            From ! {commit, minus, self()},
            To ! {commit, plus, self()}
    end,
    transaction_runner(From, To, Prepared).
