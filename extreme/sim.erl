-module(sim).
-export([run/2]).

run(NbBanks, NbClientsPerBank) ->
    Coordinator = spawn(coordinator, serve, []),
    Banks = create_banks(NbBanks, Coordinator),
    Clients = create_clients(NbClientsPerBank, Banks),
    % io:format("Begin~n"),
    % display_banks(Banks),
    lists:foreach(fun(Client) -> lists:foreach(fun(Other) ->
                        case Client /= Other of
                            true -> Client ! {transfer, new, Other};
                            false -> ok
                        end
                                               end, Clients)
                  end, Clients),
    wait_end([], Clients).
    % io:format("End~n"),
    % display_banks(Banks).

create_banks(Nb, Coordinator) ->
    lists:map(fun(_) -> spawn(bank, serve, [Coordinator]) end, lists:duplicate(Nb, ok)).

create_clients(Nb, Banks) ->
    lists:foldr(fun(A, B) -> A ++ B end, [],
                lists:map(fun(Bank) ->
                                      lists:map(fun(_) -> spawn(client, serve, [Bank]) end, lists:duplicate(Nb, ok))
                              end, Banks)).

wait_end([], []) -> ok;
wait_end([], Remain) ->
    lists:foreach(fun(Client) -> Client ! {transfer, remaining, self()} end, Remain),
    wait_end(Remain, []);
wait_end(Wait, Remain) ->
    receive
        {client, remain, Client, 0} -> wait_end(Wait -- [Client], Remain);
        {client, remain, Client, _} -> wait_end(Wait -- [Client], [Client|Remain])
    end.

% display_banks(Banks) -> lists:foreach(fun(B) -> B ! display end, Banks).
