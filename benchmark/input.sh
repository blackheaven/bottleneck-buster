#!/usr/local/bin/zsh

erlc *

for b in `seq 1 5`
do
    echo -n "<tr><th>$b</th>"
    for c in `seq 2 2 10`
    do
        echo -n "<td>"
        erl -noshell -eval "sim:run(1,1), sim:run($b, $c), {A, _} = timer:tc(sim, run, [$b, $c]), {B, _} = timer:tc(sim, run, [$b, $c]), {C, _} = timer:tc(sim, run, [$b, $c]), io:format(\"~.3f\", [(A + B + C)/3000000])." -s init stop
        echo -n "</td>"
    done
    echo "</tr>"
done
