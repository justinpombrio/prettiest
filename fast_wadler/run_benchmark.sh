#!/bin/bash

chitil_sizes="20 40 60 80 100 120 140"
huge_sizes="20 21 22 23 24 25 26"
antagonistic_sizes="200 400 600 800 1000 1500 2000 2500 3000"
nested_list_sizes="200 400 600 800 1000 1500 2000"
incremental_sizes="15 16 17 18 19 20"
exponential_sizes="12 13 14 15 16 17"

function testit {
    local impl="$1"
    shift
    local testcase="$1"
    shift
    local size="$1"
    shift
    echo -n "test $impl $testcase $size -> "
    ./Benchmark test "$impl" "$testcase" "$size"
}

function timeit {
    local impl="$1"
    shift
    local testcase="$1"
    shift
    echo ""
    echo "Test case $testcase"
    echo "Test case $testcase" | tr -c '\n' '-'
    echo -e "Size\tTime(s)"

    local size runtime
    for size in "$@"; do
        runtime="$(
          command time -f "%U" "./Benchmark" "run" "$impl" "$testcase" "$size" 2>&1 >/dev/null
        )"
        echo -e "$size\t$runtime"
    done
}

ghc --make Benchmark.hs || exit 1

echo ""
echo "┏━━━━━━━━━┓"
echo "┃ Testing ┃"
echo "┗━━━━━━━━━┛"

echo ""
testit Chitil chitil 5
testit Chitil huge 5
testit Chitil antagonistic 5
testit Chitil nestedLists 5
testit Chitil incremental 3

echo ""
# testit Chitil all 0
# testit Chitil all 1
# testit Chitil all 2
# testit Chitil all 3
# testit Chitil all 4
# testit Chitil all 5
# testit Chitil random 10
# testit Chitil random 25
# testit Chitil random 100

echo ""
testit Pombrio chitil 5
testit Pombrio huge 5
testit Pombrio antagonistic 5
testit Pombrio nestedLists 5
testit Pombrio incremental 3

echo ""
testit Pombrio all 0
testit Pombrio all 1
testit Pombrio all 2
testit Pombrio all 3
testit Pombrio all 4
testit Pombrio all 5
testit Pombrio random 10
testit Pombrio random 25
testit Pombrio random 100

echo ""
# TODO: fails with annoyingly large output
#testit Swierstra chitil 5
testit Swierstra huge 5
testit Swierstra antagonistic 5
testit Swierstra nestedLists 5
testit Swierstra incremental 3

echo ""
# testit Swierstra all 0
# testit Swierstra all 1
# testit Swierstra all 2
# testit Swierstra all 3
# testit Swierstra all 4
# testit Swierstra all 5
# testit Swierstra random 10
# testit Swierstra random 25
# testit Swierstra random 100

echo ""
echo "┏━━━━━━━━━━━┓"
echo "┃ Chitil.hs ┃"
echo "┗━━━━━━━━━━━┛"

timeit Chitil chitil $chitil_sizes
timeit Chitil huge $huge_sizes
timeit Chitil antagonistic $antagonistic_sizes
timeit Chitil nestedLists $nested_list_sizes
timeit Chitil incremental $incremental_sizes
timeit Chitil exponential $exponential_sizes

echo ""
echo "┏━━━━━━━━━━━━━━┓"
echo "┃ Swierstra.hs ┃"
echo "┗━━━━━━━━━━━━━━┛"

timeit Swierstra chitil $chitil_sizes
timeit Swierstra huge $huge_sizes
timeit Swierstra antagonistic $antagonistic_sizes
timeit Swierstra nestedLists $nested_list_sizes
timeit Swierstra incremental $incremental_sizes
timeit Swierstra exponential $exponential_sizes

echo ""
echo "┏━━━━━━━━━━━━┓"
echo "┃ Pombrio.hs ┃"
echo "┗━━━━━━━━━━━━┛"

timeit Pombrio chitil $chitil_sizes
timeit Pombrio huge $huge_sizes
timeit Pombrio antagonistic $antagonistic_sizes
timeit Pombrio nestedLists $nested_list_sizes
timeit Pombrio incremental $incremental_sizes
timeit Pombrio exponential $exponential_sizes

echo ""
echo "┏━━━━━━━━━━━┓"
echo "┃ Wadler.hs ┃"
echo "┗━━━━━━━━━━━┛"

timeit Wadler chitil $chitil_sizes
timeit Wadler huge $huge_sizes
timeit Wadler antagonistic $antagonistic_sizes
timeit Wadler nestedLists $nested_list_sizes
timeit Wadler incremental $incremental_sizes
timeit Wadler exponential $exponential_sizes
