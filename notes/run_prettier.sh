#!/bin/bash

chitil_sizes="20 40 60 80 100 120 140"
huge_sizes="20 21 22 23 24 25 26 27 28 29 30"
antagonistic_sizes="200 400 600 800 1000 1500 2000 2500 3000 3500 4000 4500 5000 6000 7000
8000 9000 10000"
nested_list_sizes="200 400 600 800 1000 1500 2000 2500 3000 3500 4000 4500 5000"

function run_test {
    local exec="$1"
    shift
    local name="$1"
    shift
    echo ""
    echo "$name" | tr a-z A-Z
    echo -e "Size\tTime(s)"

    local size runtime
    for size in "$@"; do
        runtime="$(
          command time -f "%U" "./$exec" "$name" "$size" 2>&1 >/dev/null
        )"
        echo -e "$size\t$runtime"
    done
}

ghc --make prettier.hs || exit 1
ghc --make lazy_dequeue.hs || exit 1

echo "NOTE: In CHITIL, the size is the pretty printing width. In the rest, it is the size of the
document."

echo ""
echo "lazy_dequeue.hs"
echo "==============="
run_test lazy_dequeue chitil $chitil_sizes

echo ""
echo "prettier.hs"
echo "==========="
run_test prettier chitil $chitil_sizes
run_test prettier huge $huge_sizes
run_test prettier antagonistic $antagonistic_sizes
run_test prettier nestedLists $nested_list_sizes
