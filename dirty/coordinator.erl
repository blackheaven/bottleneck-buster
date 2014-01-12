-module(coordinator).
-export([serve/0]).

serve() ->
    serve([], [], [], []).

serve(Hosts, Searching, PreparationAsked, Commiting) ->
    receive
        {host, add, PID} ->
            serve([PID|Hosts], Searching, PreparationAsked, Commiting);
        {host, delete, PID} ->
            serve(Hosts -- [PID], Searching, PreparationAsked, Commiting);
        {client, transfer, From, ToC} -> % From = {Client, Bank}
            lists:foreach(fun(H) -> H ! {client, has, ToC, self()} end, Hosts),
            serve(Hosts, [{From, ToC}|Searching], PreparationAsked, Commiting);
        {client, founded, Client, Bank} ->
            Founded = lists:filter(fun({_, C}) -> C == Client end, Searching),
            PreparationToAsk = lists:map(fun({F, T}) -> {F, {T, Bank}} end, Founded),
            lists:foreach(fun({{FC, FB}, {TC, _}}) -> FB ! {client, prepare, FC, TC} end, PreparationToAsk),
            serve(Hosts,
                  lists:filter(fun({_, C}) -> C /= Client end, Searching),
                  PreparationAsked ++ PreparationToAsk,
                  Commiting);
        {client, preparation, error, FC, TC} ->
            PreparationErrors = lists:filter(fun({{F, _}, {T, _}}) -> (F == FC) and (T == TC) end, PreparationAsked),
            lists:foreach(fun({{F, _}, {T, _}}) -> F ! {transfer, error, T} end, PreparationErrors),
            PreparationWaiting = lists:filter(fun({{F, _}, {T, _}}) -> (F /= FC) or (T /= TC) end, PreparationAsked),
            serve(Hosts, Searching, PreparationWaiting, Commiting);
        {client, preparation, ok, FC, TC} ->
            PreparationOk = lists:filter(fun({{F, _}, {T, _}}) -> (F == FC) and (T == TC) end, PreparationAsked),
            lists:foreach(fun({{FCl, FBl}, {TCl, TBl}}) ->
                                  FBl ! {client, commit, FCl, minus, TCl},
                                  TBl ! {client, commit, TCl, plus, FCl}
                          end, PreparationOk),
            PreparationWaiting = lists:filter(fun({{F, _}, {T, _}}) -> (F /= FC) or (T /= TC) end, PreparationAsked),
            serve(Hosts, Searching, PreparationWaiting, Commiting ++ PreparationOk);
        {client, commit, error, FC,  minus, TC} ->
            CommitError = lists:filter(fun({{F, _}, {T, _}}) -> (F == FC) and (T == TC) end, Commiting),
            lists:foreach(fun({_, {TCl, TBl}}) -> TBl ! {client, rollback, TCl} end, CommitError),
            FC ! {transfer, error, TC},
            serve(Hosts, Searching, PreparationAsked, Commiting -- CommitError);
        {client, commit, ok, FC,  minus, TC} ->
            CommitOk = lists:filter(fun({{F, _}, {T, _}}) -> (F == FC) and (T == TC) end, Commiting),
            FC ! {transfer, ok, TC},
            serve(Hosts, Searching, PreparationAsked, Commiting -- CommitOk);
        _ ->
            serve(Hosts, Searching, PreparationAsked, Commiting)
    end.


