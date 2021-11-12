#!/bin/bash

huge_sizes="20 21 22 23 24 25 26 27 28 29 30"
antagonistic_sizes="200 400 600 800 1000 1500 2000 2500 3000 3500 4000 4500 5000 6000 7000
8000 9000 10000"
nested_list_sizes="200 400 600 800 1000 1500 2000 2500 3000 3500 4000 4500 5000"

function run_test {
    local name="$1"
    shift
    echo ""
    echo "$name" | tr a-z A-Z
    echo -e "Size\tTime(s)"

    local size runtime
    for size in "$@"; do
        runtime="$(
          command time -f "%U" ./prettier "$name" "$size" 2>&1 >/dev/null
        )"
        echo -e "$size\t$runtime"
    done
}

run_test huge $huge_sizes
run_test antagonistic $antagonistic_sizes
run_test nestedLists $nested_list_sizes
