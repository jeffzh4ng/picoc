#!/bin/bash

compile_and_eval() {
  input="$1"
  compiler="$2"

  if [ "$compiler" = "din" ]; then
    ./target/release/picoc089 compilec89 "$input" O0 > /dev/null
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
    echo "EXPECTED gcc: $gcc_result, GOT din: $din_result"
    exit 1
  fi
}

#
#
#
# **********************************************************************************************************************
# *************************************************** SANITY CHECKS ****************************************************
# **********************************************************************************************************************

# --- arithmetic ---
assert "./tests/fixtures/snap/shared/arith/lit.c"
assert "./tests/fixtures/snap/shared/arith/add.c"
assert "./tests/fixtures/snap/shared/arith/add_multi.c"
assert "./tests/fixtures/snap/shared/arith/sub.c"
# assert "./tests/fixtures/snap/shared/arith/mult.c"
# assert "./tests/fixtures/snap/shared/arith/div.c"

assert "./tests/fixtures/snap/shared/arith/add_associative.c"
assert "./tests/fixtures/snap/shared/arith/sub_associative.c"
# assert "./tests/fixtures/legal/snap/shared/arith/mult_add_precedence.c"
# assert "./tests/fixtures/legal/arithmetic_precedence/mult_add_precedence_multi.c"

# --- control flow ---
# assert "./tests/fixtures/legal/control_flow/eq_true.c"
# assert "./tests/fixtures/legal/control_flow/eq_false.c"
# assert "./tests/fixtures/legal/control_flow/neq_true.c"
# assert "./tests/fixtures/legal/control_flow/neq_false.c"

# assert "./tests/fixtures/legal/control_flow/and_true.c"
# assert "./tests/fixtures/legal/control_flow/or_true.c"
# assert "./tests/fixtures/legal/control_flow/and_false.c"
# assert "./tests/fixtures/legal/control_flow/or_false.c"

# assert "./tests/fixtures/legal/control_flow/lt_true.c"
# assert "./tests/fixtures/legal/control_flow/lteq_true.c"
# assert "./tests/fixtures/legal/control_flow/lteq2_true.c"
# assert "./tests/fixtures/legal/control_flow/gt_true.c"
# assert "./tests/fixtures/legal/control_flow/gteq_true.c"
# assert "./tests/fixtures/legal/control_flow/gteq2_true.c"

# assert "./tests/fixtures/legal/control_flow/ifels_then.c"
# assert "./tests/fixtures/legal/control_flow/ifels_els.c"

# assert "./tests/fixtures/legal/control_flow/for.c"

# --- bindings ---
# assert "./tests/fixtures/legal/data_flow/asnmt.c"
# assert "./tests/fixtures/legal/data_flow/asnmt_multi.c"
# assert "./tests/fixtures/legal/data_flow/asnmt_multi_expr.c"
# assert "./tests/fixtures/legal/data_flow/asnmt_multi_expr_var.c"
# assert "./tests/fixtures/legal/data_flow/asnmt_update.c"
# assert "./tests/fixtures/legal/data_flow/asnmt_update_inc.c"
# assert "./tests/fixtures/legal/data_flow/asnmt_update_dec.c"

# -- functions

## --- heap ---
# -- malloc/free
# -- pointer/deref
# -- structs selec/deref
# -- alloc/dealloc fixed sized arrays

#
#
#
# **********************************************************************************************************************
# ******************************************* DATA STRUCTURES & ALGORITHMS *********************************************
# **********************************************************************************************************************

# map/set
# list/stack/queue/priority queue
# tree/graph

echo "picoc-test: all tests completed."
