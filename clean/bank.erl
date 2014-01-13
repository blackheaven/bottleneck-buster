-module(bank).
-export([serve/1]).

serve(Coordinator) ->
    Coordinator ! {host, add, self()},
    serve(Coordinator, []).

serve(Coordinator, Clients) ->
    receive
        {client, transfer, From, To, Asker} ->
            case exist(From, Clients) of
                false -> Asker ! not_a_client;
                true -> Coordinator ! {client, transfer, To, Asker}
            end,
            serve(Coordinator, Clients);
        {client, prepare, From, Asker} ->
            Asker ! case exist(From, Clients) of
                false -> not_a_client;
                true ->
                    [{_, Cash}|_] = find(From, Clients),
                    case Cash > 0 of
                        false -> error;
                        true -> prepared
                    end
            end,
            serve(Coordinator, Clients);
        {client, commit, From, Op, Asker} ->
            case exist(From, Clients) of
                false ->
                    Asker ! error,
                    serve(Coordinator, Clients);
                true ->
                    [{_, Cash}|_] = find(From, Clients),
                    case Op of
                        minus ->
                            case Cash > 0 of
                                false ->
                                    Asker ! error,
                                    serve(Coordinator, Clients);
                                true ->
                                    Asker ! ok,
                                    serve(Coordinator, [{From, Cash - 1}|lists:filter(fun({P, _}) -> P /= From end, Clients)])
                            end;
                        plus ->
                            serve(Coordinator, [{From, Cash + 1}|lists:filter(fun({P, _}) -> P /= From end, Clients)])
                    end
            end;
        {client, rollback, From} ->
            case exist(From, Clients) of
                false ->
                    serve(Coordinator, Clients);
                true ->
                    [{_, Cash}|_] = find(From, Clients),
                    serve(Coordinator, [{From, Cash - 1}|lists:filter(fun({P, _}) -> P /= From end, Clients)])
            end;
        {client, add, PID} ->
            serve(Coordinator,
                case exist(PID, Clients) of
                    false -> [{PID, 10}|Clients];
                    true -> Clients
                end);
        {client, has, PID, Asker} ->
            case exist(PID, Clients) of
                false -> not_a_client;
                true -> Asker ! {founded, self()}
            end,
            serve(Coordinator, Clients);
        {client, delete, PID} ->
            serve(Coordinator, lists:filter(fun({P, _}) -> P /= PID end, Clients));
        display ->
            io:format("~p : ~w~n", [self(),
                                    lists:sort(fun({A, _},{B, _}) -> A < B end
                                      , Clients)]),
            serve(Coordinator, Clients);
        _ ->
            io:format("~p : H~n", [self()]),
            serve(Coordinator, Clients)
    end.

exist(E, L) -> length(find(E, L)) /= 0.
find (E, L) -> lists:filter(fun({P, _}) -> P == E end, L).
