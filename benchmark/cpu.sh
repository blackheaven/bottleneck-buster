#!/usr/local/bin/zsh

erlc *
b=8
c=10

for c in `seq 1 8`
do
    echo -n "<td>"
    erl -S $n:$n -noshell -eval "sim:run(1,1), sim:run($b, $c), {A, _} = timer:tc(sim, run, [$b, $c]), {B, _} = timer:tc(sim, run, [$b, $c]), {C, _} = timer:tc(sim, run, [$b, $c]), io:format(\"~.3f\", [(A + B + C)/3000000])." -s init stop
    echo -n "</td>"
done
