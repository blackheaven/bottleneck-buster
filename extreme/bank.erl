-module(bank).
-export([serve/1, account/1]).

serve(Coordinator) ->
    Coordinator ! {host, add, self()},
    serve(Coordinator, []).

serve(Coordinator, Clients) ->
    receive
        {client, transfer, From, To, Asker} ->
            case exist(From, Clients) of
                false -> Asker ! not_a_client;
                true ->
                    [{_, Account}] = find(From, Clients),
                    Asker ! {from, Account},
                    Coordinator ! {client, transfer, To, Asker}
            end,
            serve(Coordinator, Clients);
        {client, add, PID} ->
            serve(Coordinator,
                case exist(PID, Clients) of
                    false -> [{PID, spawn(?MODULE, account, [10])}|Clients];
                    true -> Clients
                end);
        {client, has, PID, Asker} ->
            case exist(PID, Clients) of
                false -> not_a_client;
                true ->
                    [{_, Account}] = find(PID, Clients),
                    Asker ! {founded, Account}
            end,
            serve(Coordinator, Clients);
        {client, delete, PID} ->
            serve(Coordinator, lists:filter(fun({P, _}) -> P /= PID end, Clients));
        display ->
            lists:foreach(fun({_, A}) -> A ! display end, Clients),
            serve(Coordinator, Clients);
        _ ->
            io:format("~p : H~n", [self()]),
            serve(Coordinator, Clients)
    end.

exist(E, L) -> length(find(E, L)) /= 0.
find (E, L) -> lists:filter(fun({P, _}) -> P == E end, L).

account(Sold) ->
    receive
        {prepare, Asker} ->
            Asker ! case Sold > 0 of
                        false -> error;
                        true -> prepared
                    end,
            account(Sold);
        {commit, Op, Asker} ->
            case Op of
                minus ->
                    case Sold > 0 of
                        false ->
                            Asker ! error,
                            account(Sold);
                        true ->
                            Asker ! ok,
                            account(Sold - 1)
                    end;
                plus ->
                    account(Sold + 1)
            end;
        rollback ->
            account(Sold - 1);
        display ->
            io:format("~p : ~p~n", [self(), Sold]),
            account(Sold)
    end.
