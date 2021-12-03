#!/bin/bash

chitil_sizes="20 40 60 80 100 120 140"
huge_sizes="20 21 22 23 24 25 26"
antagonistic_sizes="200 400 600 800 1000 1500 2000 2500 3000"
nested_list_sizes="200 400 600 800 1000 1500 2000"

function testit {
    local impl="$1"
    shift
    local testcase="$1"
    shift
    local size="$1"
    shift
    echo -n "test $impl $testcase $size -> "
    ./benchmark test "$impl" "$testcase" "$size"
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
          command time -f "%U" "./benchmark" "run" "$impl" "$testcase" "$size" 2>&1 >/dev/null
        )"
        echo -e "$size\t$runtime"
    done
}

ghc --make benchmark.hs || exit 1

echo ""
echo "┏━━━━━━━━━┓"
echo "┃ Testing ┃"
echo "┗━━━━━━━━━┛"

echo ""
testit Chitil chitil 20
testit Chitil huge 20
testit Chitil antagonistic 20
testit Chitil nestedLists 20

echo ""
testit Chitil all 0
testit Chitil all 1
testit Chitil all 2
testit Chitil all 3
testit Chitil all 4
testit Chitil all 5

echo ""
testit Pombrio chitil 20
testit Pombrio huge 20
testit Pombrio antagonistic 20
testit Pombrio nestedLists 20

echo ""
testit Pombrio all 0
testit Pombrio all 1
testit Pombrio all 2
testit Pombrio all 3
testit Pombrio all 4
testit Pombrio all 5

echo ""
echo "┏━━━━━━━━━━━┓"
echo "┃ Chitil.hs ┃"
echo "┗━━━━━━━━━━━┛"

timeit Chitil chitil $chitil_sizes
timeit Chitil huge $huge_sizes
timeit Chitil antagonistic $antagonistic_sizes
timeit Chitil nestedLists $nested_list_sizes

echo ""
echo "┏━━━━━━━━━━━━┓"
echo "┃ Pombrio.hs ┃"
echo "┗━━━━━━━━━━━━┛"

timeit Pombrio chitil $chitil_sizes
timeit Pombrio huge $huge_sizes
timeit Pombrio antagonistic $antagonistic_sizes
timeit Pombrio nestedLists $nested_list_sizes

echo ""
echo "┏━━━━━━━━━━━┓"
echo "┃ Wadler.hs ┃"
echo "┗━━━━━━━━━━━┛"

timeit Wadler chitil $chitil_sizes
timeit Wadler huge $huge_sizes
timeit Wadler antagonistic $antagonistic_sizes
timeit Wadler nestedLists $nested_list_sizes
