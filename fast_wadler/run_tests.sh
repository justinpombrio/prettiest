#!/bin/bash

function testit {
    local impl="$1"
    shift
    local testcase="$1"
    shift
    local size="$1"
    shift
    echo -n "test $impl $testcase $size -> "
    ./RunPretty test "$impl" "$testcase" "$size"
}

ghc --make RunPretty.hs -O2 || exit 1

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
testit Pombrio all 0
testit Pombrio all 1
testit Pombrio all 2
testit Pombrio all 3
testit Pombrio all 4
testit Pombrio all 5

echo ""
testit Pombrio chitil 5
testit Pombrio huge 5
testit Pombrio antagonistic 5
testit Pombrio nestedLists 5
testit Pombrio incremental 3

echo ""
testit Pombrio random 10
testit Pombrio random 20
testit Pombrio random 50
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
