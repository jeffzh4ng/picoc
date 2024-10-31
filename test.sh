#!/bin/bash

compile_and_eval() {
  input="$1"
  compiler="$2"

  if [ "$compiler" = "din" ]; then
    ./target/release/din "$input" > tmp.s || exit
    riscv64-unknown-elf-gcc -o tmp tmp.s
  elif [ "$compiler" = "gcc" ]; then
    riscv64-unknown-elf-gcc -o tmp "$input"
  # elif [ "$compiler" = "clang" ]; then
  #   riscv64-unknown-elf-clang -o tmp "$input"
  else
    echo "picoc-test: error, unknown compiler: $compiler"
    exit 1
  fi

  spike pk tmp
  echo $?
}

assert() {
  input="$1"
  din_result=$(compile_and_eval "$input" "din")
  gcc_result=$(compile_and_eval "$input" "gcc")

  if [ "$din_result" != "$gcc_result" ]; then
    echo "[$input] din: $din_result, gcc: $gcc_result - MISMATCH"
    exit 1
  fi
}

# ---tests---
# 1. expressions
# 2. control flow
# 3. data transfer
# 5. structs
# 4. malloc/free

# --- expressions ---
assert "./tests/fixtures/legal/arithmetic/lit.c"
assert "./tests/fixtures/legal/arithmetic/add.c"
assert "./tests/fixtures/legal/arithmetic/add_multi.c"
assert "./tests/fixtures/legal/arithmetic/sub.c"
assert "./tests/fixtures/legal/arithmetic/mult.c"
assert "./tests/fixtures/legal/arithmetic/div.c"

assert "./tests/fixtures/legal/arithmetic_precedence/add_associative.c"
assert "./tests/fixtures/legal/arithmetic_precedence/sub_associative.c"
assert "./tests/fixtures/legal/arithmetic_precedence/mult_add_precedence.c"
assert "./tests/fixtures/legal/arithmetic_precedence/mult_add_precedence_multi.c"

# --- control flow ---
# -- booleans
assert "./tests/fixtures/legal/control_flow/eq_true.c"
assert "./tests/fixtures/legal/control_flow/eq_false.c"
assert "./tests/fixtures/legal/control_flow/neq_true.c"
assert "./tests/fixtures/legal/control_flow/neq_false.c"

assert "./tests/fixtures/legal/control_flow/and_true.c"
assert "./tests/fixtures/legal/control_flow/or_true.c"
assert "./tests/fixtures/legal/control_flow/and_false.c"
assert "./tests/fixtures/legal/control_flow/or_false.c"

assert "./tests/fixtures/legal/control_flow/lt_true.c"
assert "./tests/fixtures/legal/control_flow/lteq_true.c"
assert "./tests/fixtures/legal/control_flow/lteq2_true.c"
assert "./tests/fixtures/legal/control_flow/gt_true.c"
assert "./tests/fixtures/legal/control_flow/gteq_true.c"
assert "./tests/fixtures/legal/control_flow/gteq2_true.c"

# -- conditionals
assert "./tests/fixtures/legal/control_flow/ifels_then.c"
assert "./tests/fixtures/legal/control_flow/ifels_els.c"

# -- loops
assert "./tests/fixtures/legal/control_flow/for.c"

# -- functions

# --- data transfer ---
# -- bindings
assert "./tests/fixtures/legal/data_flow/asnmt.c"
assert "./tests/fixtures/legal/data_flow/asnmt_multi.c"
assert "./tests/fixtures/legal/data_flow/asnmt_multi_expr.c"
assert "./tests/fixtures/legal/data_flow/asnmt_multi_expr_var.c"
assert "./tests/fixtures/legal/data_flow/asnmt_update.c"
assert "./tests/fixtures/legal/data_flow/asnmt_update_inc.c"
assert "./tests/fixtures/legal/data_flow/asnmt_update_dec.c"

# -- malloc/free
# -- pointer/deref
# -- structs selec/deref
# -- alloc/dealloc fixed sized arrays

echo "picoc-test: all tests completed."
