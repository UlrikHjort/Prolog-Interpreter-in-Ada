#!/bin/bash
# Test runner for Ada Prolog Interpreter

PROLOG=./bin/prolog
TESTS=./test

echo "Ada Prolog Interpreter - Test Suite"
echo "===================================="
echo

passed=0
failed=0

run_test() {
    name=$1
    file=$2
    expected=$3

    output=$( (cat "$file"; echo "") | $PROLOG 2>&1 )
    # Extract the result line (after ?- , contains = or true or false)
    result=$(echo "$output" | grep "^?-" | grep -v "Clause added" | grep -v "Bye" | sed 's/^?- //' | head -1)

    if echo "$result" | grep -qF "$expected"; then
        echo "[PASS] $name: $result"
        ((passed++))
    else
        echo "[FAIL] $name"
        echo "       Expected: $expected"
        echo "       Got: $result"
        ((failed++))
    fi
}

# Basic tests
run_test "Basic fact query" "$TESTS/test_basic.pl" "true"
run_test "Unification same/2" "$TESTS/test_unification.pl" "true"

# Arithmetic tests
run_test "Arithmetic: 3+4=7" "$TESTS/test_arithmetic.pl" "X = 7"
run_test "Factorial(5)=120" "$TESTS/test_factorial.pl" "X = 120"
run_test "Max comparison" "$TESTS/test_comparison.pl" "M = 5"

# List tests
run_test "List append" "$TESTS/test_lists.pl" "X = [1, 2, 3, 4]"
run_test "List length" "$TESTS/test_length.pl" "N = 5"
run_test "Member found" "$TESTS/test_member.pl" "true"
run_test "Member not found" "$TESTS/test_notmember.pl" "false"

# Relation tests
run_test "Grandparent relation" "$TESTS/test_family.pl" "true"

# I/O tests
run_test "Write predicate" "$TESTS/test_write.pl" "hello"

# Floating point tests
run_test "Float addition" "$TESTS/test_float.pl" "X = 5"
run_test "Float division" "$TESTS/test_float_div.pl" "true"
run_test "Float sqrt" "$TESTS/test_float_sqrt.pl" "X = 5"
run_test "Float type checking" "$TESTS/test_float_type.pl" "true"

echo
echo "===================================="
echo "Results: $passed passed, $failed failed"

if [ $failed -gt 0 ]; then
    exit 1
fi
