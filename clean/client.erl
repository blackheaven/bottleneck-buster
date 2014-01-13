-module(client).
-export([serve/1,transaction_runner/5]).

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
    Tid = spawn(?MODULE, transaction_runner, [From, To, Bank, false, false]),
    Bank ! {client, transfer, From, To, Tid},
    Bank ! {client, prepare, From, Tid},
    % io:format("U ~p~n", [Tid]),
    Tid.

transaction_runner(From, To, Bank, Prepared, TBank) ->
    receive
        error ->
            case (TBank /= false) and Prepared of
                false -> ok;
                true -> TBank ! {client, rollback, To}
            end,
            Bank ! {client, prepare, From, self()},
            transaction_runner(From, To, Bank, false, TBank);
        prepared -> walk(From, To, Bank, true, TBank);
        {founded, NBank} -> walk(From, To, Bank, Prepared, NBank);
        ok -> ok;
        not_a_client -> not_a_client
    end.

walk(From, To, Bank, Prepared, TBank) ->
    case (TBank /= false) and Prepared of
        false -> ok;
        true ->
            Bank ! {client, commit, From, minus, self()},
            TBank ! {client, commit, To, plus, self()}
    end,
    transaction_runner(From, To, Bank, Prepared, TBank).
