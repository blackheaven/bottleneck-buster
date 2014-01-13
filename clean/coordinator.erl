-module(coordinator).
-export([serve/0]).

serve() ->
    serve([]).

serve(Hosts) ->
    receive
        {host, add, PID} ->
            serve([PID|Hosts]);
        {host, delete, PID} ->
            serve(Hosts -- [PID]);
        {client, transfer, ToC, Asker} ->
            lists:foreach(fun(H) -> H ! {client, has, ToC, Asker} end, Hosts),
            serve(Hosts);
        _ ->
            serve(Hosts)
    end.


