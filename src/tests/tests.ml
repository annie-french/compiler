(** This file should be used to write your tests of your other code. *)

open Batteries;;

open OUnit2;;

open TestUtils;;

let all_tests =
  [
    test_success "test_code/4.bird" "4";
    test_success "test_code/after_4.bird" "5";
    test_success "test_code/before_4.bird" "3";
    test_success "test_code/before_after_4.bird" "4";
    test_success "test_code/minus1.bird" "3";
    test_success "test_code/plus1.bird" "2";
    test_success "test_code/plus3.bird" "6";
    test_success "test_code/times1.bird" "8";
    test_success "test_code/times2.bird" "15";
    test_success "test_code/arithmetic.bird" "-9";
    test_success "test_code/arithmetic_2.bird" "14";
    test_success "test_code/let1.bird" "1";
    test_success "test_code/let2.bird" "10";
    test_success "test_code/is_int_6.bird" "true";
    test_success "test_code/is_int_false.bird" "false";
    test_success "test_code/is_bool_true.bird" "true";
    test_success "test_code/less_than.bird" "false";
    test_success "test_code/greater_than.bird" "true";
    test_success "test_code/equal.bird" "false";
    test_success "test_code/working_if.bird" "7";
    test_success "test_code/false_if.bird" "false";
    test_success "test_code/big_multiplication.bird" "4000000000000000000";
    test_success "test_code/nested_and.bird" "false";
    test_success "test_code/nested_or.bird" "true";
    test_success "test_code/print_4.bird" "4\n4";
    test_success "test_code/print_nested.bird" "4\nfalse\nfalse";
    test_success "test_code/print_before.bird" "3\n-14";
    test_runtime_failure "test_code/fail_after.bird" 1;
    test_runtime_failure "test_code/fail_arithmetic.bird" 1;
    test_runtime_failure "test_code/fail_if.bird" 2;
    test_runtime_failure "test_code/fail_greater.bird" 1;
    test_runtime_failure "test_code/fail_and.bird" 2;
    test_success "test_code/fn_simple.bird" "1";
    test_success "test_code/fn_1.bird" "true";
    test_success "test_code/fn_2.bird" "1";
    test_success "test_code/fn_recursion.bird" "15";
    test_success "test_code/print_fn.bird" "1\n2\n3";
    test_compile_failure "test_code/fcn_undefined.bird" "Unbound variable g";
    test_compile_failure "test_code/duplicate_param.bird" "Function f declares a duplicate parameter x";
    test_compile_failure "test_code/duplicate_fn.bird" "Duplicate definition of function f\nDuplicate definition of function g";
    test_compile_failure "test_code/unbound_var.bird" "Unbound variable y";
    test_success "test_code/basic_tuple.bird" "(1, 4)";
    test_success "test_code/is_tuple_true.bird" "true";
    test_success "test_code/is_tuple_false.bird" "false";
    test_success "test_code/tuple_index.bird" "1";
    test_success "test_code/tuple_index_2.bird" "(2, 12)";
    test_success "test_code/nested_tuple.bird" "(2, true)";
    test_runtime_failure "test_code/tuple_error.bird" 3;
    test_runtime_failure "test_code/index_error.bird" 4;
    test_success "test_code/mutation1.bird" "12";
    test_success "test_code/seg_fault_8.bird" "2";
    test_success "test_code/seg_fault_16.bird" "(((1, 3), (4, 5)), ((1, 3), (4, 5)))";
    test_success "test_code/seg_fault_32.bird" "((1, 2, 3), (1, 2, 3), (1, 2, 3), (1, 2, 3))";
    test_success "test_code/cycle_tuple.bird" "1048576";
    test_runtime_failure "test_code/use_tuple.bird" 7;
    test_success "test_code/cycle_closure.bird" "1048576";
    test_runtime_failure "test_code/use_closure.bird" 7;
    ] 
  ;;

let suite = "compiler test suite" >::: all_tests;;

run_test_tt_main suite;;
