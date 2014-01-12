-module(bank).
-export([serve/1]).

serve(Coordinator) ->
    Coordinator ! {host, add, self()},
    serve(Coordinator, []).

serve(Coordinator, Clients) ->
    % io:format("~p : ~p~n", [self(), Clients]),
    receive
        {client, transfer, From, To} ->
            case exist(From, Clients) of
                false -> From ! {error, not_a_client};
                true -> Coordinator ! {client, transfer, {From, self()}, To}
            end,
            serve(Coordinator, Clients);
        {client, prepare, From, To} ->
            Coordinator ! case exist(From, Clients) of
                false -> {client, preparation, error, From, To};
                true ->
                    [{_, Cash}|_] = find(From, Clients),
                    case Cash > 0 of
                        false -> {client, preparation, error, From, To};
                        true -> {client, preparation, ok, From, To}
                    end
            end,
            serve(Coordinator, Clients);
        {client, commit, From, Op, To} ->
            case exist(From, Clients) of
                false ->
                    Coordinator ! {client, commit, error, From, Op, To},
                    serve(Coordinator, Clients);
                true ->
                    [{_, Cash}|_] = find(From, Clients),
                    case Op of
                        minus ->
                            case Cash > 0 of
                                false ->
                                    Coordinator ! {client, commit, error, From, Op, To},
                                    serve(Coordinator, Clients);
                                true ->
                                    Coordinator ! {client, commit, ok, From, Op, To},
                                    serve(Coordinator, [{From, Cash - 1}|lists:filter(fun({P, _}) -> P /= From end, Clients)])
                            end;
                        plus ->
                            Coordinator ! {client, commit, ok, From, Op, To},
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
                    false -> [{PID, 100}|Clients];
                    true -> Clients
                end);
        {client, has, PID, Asker} ->
            case exist(PID, Clients) of
                false -> not_a_client;
                true -> Asker ! {client, founded, PID, self()}
            end,
            serve(Coordinator, Clients);
        {client, delete, PID} ->
            serve(Coordinator, lists:filter(fun({P, _}) -> P /= PID end, Clients));
        _ ->
            serve(Coordinator, Clients)
    end.

exist(E, L) -> length(find(E, L)) /= 0.
find (E, L) -> lists:filter(fun({P, _}) -> P == E end, L).
