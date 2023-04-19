(*
 * TODO:
 *    Creating the string type
 *    Declare the print function
 *    Define the print function
 *    Fill in the body of the print function
 *    Construct the code for a function call expression
 *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

type queue_gep = {
  size : L.llvalue;
  cap : L.llvalue;
  array : L.llvalue;
}

type data_gep = {
  tag : L.llvalue;
  head : L.llvalue;
  tail : L.llvalue;
}

type arg_gep = {
  parent_mutex : L.llvalue;
  child_mutex : L.llvalue;
  parent_queue : L.llvalue;
  child_queue : L.llvalue;
}

let deep_copy_stringmap map =
  let new_map = StringMap.empty in
  let new_map = StringMap.fold (fun k v acc -> StringMap.add k v acc) map new_map in
  new_map

let translate ((tdecls : sthread_decl list), (fdecls : sfunc_decl list)) =
  let context = L.global_context () in
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type context in
  let pointer_t = L.pointer_type i8_t in
  let double_ptr = L.pointer_type pointer_t in
  let the_module = L.create_module context "Untangled" in

  let data_t = L.struct_type context [| i32_t; pointer_t; pointer_t |] in
  let data_ptr = L.pointer_type data_t in
  let data_double_ptr = L.pointer_type data_ptr in
  (*
   * struct Queue_t
   * @field 1 size of queue
   * @field 2 capacity of queue
   * @field 3 pointer to an array of data
   *)
  let queue_t = L.struct_type context [| i32_t; i32_t; data_double_ptr |] in
  let queue_ptr = L.pointer_type queue_t in
  let arg_t = L.struct_type context [| pointer_t; pointer_t; queue_ptr; queue_ptr |] in
  let arg_ptr = L.pointer_type arg_t in
  (* Utility to build index of gep *)
  let gep_index i = L.const_int i32_t i in

  (* Given a AST type, return a tag code *)
  let ocaml_tag = function
        A.Int -> [0]
      | A.Float -> [1]
      | A.String -> [2]
      | A.Bool -> [3]
      | _ -> raise (Failure "array tag") in
  let rec tag_pattern = function
      | SBasePattern (typ, _) -> ocaml_tag typ
      | STuplePattern (pattern1, pattern2) ->
        let tag1 = tag_pattern pattern1 in
        let tag2 = tag_pattern pattern2 in
        4 :: (tag1 @ tag2)
      | SWildcardPattern -> [6] in
  let tag_of_type = function
          A.Int -> (L.const_int i32_t 0)
        | A.Float -> (L.const_int i32_t 1)
        | A.String -> (L.const_int i32_t 2)
        | A.Bool -> (L.const_int i32_t 3)
        | A.Tuple _ -> (L.const_int i32_t 4)
        | _ -> raise (Failure " site tag")
  and lltype_of_typ = function
          A.Int   -> i32_t
        | A.Bool  -> i1_t
        | A.Float -> float_t
        | A.Void  -> void_t
        | A.String -> pointer_t
        | _ -> raise (Failure "hello")
  and cast_llvalue_to_ptr typ llvalue builder = match typ with
      A.Int -> L.build_bitcast llvalue pointer_t "int_to_ptr" builder
    | A.Float -> L.build_bitcast llvalue pointer_t "float_to_ptr" builder
    | A.String -> L.build_bitcast llvalue pointer_t "string_to_ptr" builder
    | A.Bool -> L.build_bitcast llvalue pointer_t "bool_to_ptr" builder
    | A.Tuple _ -> L.build_bitcast llvalue pointer_t "tuple_to_ptr" builder
    | _ -> raise (Failure "implement array")
  and cast_ptr_to_llvalue typ ptr builder = match typ with
      A.Int -> L.build_bitcast ptr (L.pointer_type i32_t) "int_to_ptr" builder
    | _ -> raise (Failure "Implement other conversion")
  and build_queue_gep queue builder =
    let size = L.build_in_bounds_gep queue [| gep_index 0; gep_index 0 |] "gep_size" builder and
        cap =  L.build_in_bounds_gep queue [| gep_index 0; gep_index 1 |] "gep_cap" builder and
        array = L.build_in_bounds_gep queue [| gep_index 0; gep_index 2 |] "gep_array" builder
    in { size = size; cap = cap; array = array }
  and build_data_gep data builder =
    let tag = L.build_in_bounds_gep data [| gep_index 0; gep_index 0 |] "gep_tag" builder and
        head =  L.build_in_bounds_gep data [| gep_index 0; gep_index 1 |] "gep_head" builder and
        tail = L.build_in_bounds_gep data [| gep_index 0; gep_index 2 |] "gep_tail" builder
    in { tag = tag; head = head; tail = tail }
  and build_arg_gep data builder =
    let parent_mutex = L.build_in_bounds_gep data [| gep_index 0; gep_index 0 |] "gep_parent_mutex" builder and
        child_mutex = L.build_in_bounds_gep data [| gep_index 0; gep_index 1 |] "gep_child_mutex" builder and
        parent_queue = L.build_in_bounds_gep data [| gep_index 0; gep_index 2 |] "gep_parent_queue" builder and
        child_queue = L.build_in_bounds_gep data [| gep_index 0; gep_index 3 |] "gep_child_queue" builder
    in { parent_mutex = parent_mutex;
         child_mutex = child_mutex;
         parent_queue = parent_queue;
         child_queue = child_queue }
  in

  (*
   * https://man7.org/linux/man-pages/man3/pthread_create.3.html
   *
   * pthread_create: C routine to start a thread
   * 1st argument: Address of opaque struct pthread_t
   * 2nd argument: Attribute of the thread (for example, automatic garbage collection)
   * 3rd argument: Address of the function to run
   * 4th argument: Argument to the function
   *
   * The argument passed to the function is a struct containing 2 arrays, for the parents and for
   * the child.
   *)
  let routine_t : L.lltype =
    L.function_type pointer_t [| pointer_t |] in
  (* pthread_t is an opaque struct which is a pointer *)
  let pthread_create_t : L.lltype =
    L.function_type i32_t [| double_ptr; i8_t; L.pointer_type routine_t; pointer_t |] in
  let pthread_create_func : L.llvalue =
    L.declare_function "pthread_create" pthread_create_t the_module in

  (*
   * https://man7.org/linux/man-pages/man3/pthread_join.3.html
   *
   * pthread_join: C routine to wait for a thread to finish execution
   * 1st argument: Opaque struct pthread_t
   * 2nd argment: A pointer to store the return value. In Untangled, a thread implicitly returns a
   * NULL value
   *)
  let pthread_join_t : L.lltype =
    L.function_type i32_t [| L.pointer_type i8_t; i8_t |] in
  let pthread_join_func : L.llvalue =
    L.declare_function "pthread_join" pthread_join_t the_module in

  (*
   * https://pubs.opengroup.org/onlinepubs/7908799/xsh/pthread_mutex_init.html
   *
   * pthread_mutex_init: C routine to initialize a mutex
   * 1st argument: Address of opaque struct pthread_mutex_t
   * 2nd argument: A pointer to specifiy the mutex attributes
   *)
  let pthread_mutex_init_t : L.lltype =
    L.function_type i32_t [| double_ptr; double_ptr |] in
  let pthread_mutex_init_func : L.llvalue =
    L.declare_function "pthread_mutex_init" pthread_mutex_init_t the_module in

  (*
   * https://pubs.opengroup.org/onlinepubs/7908799/xsh/pthread_mutex_lock.html
   *
   * pthread_mutex_lock: C routine to lock a mutex
   * 1st argument: Address of opaque struct pthread_mutex_t
   *)
  let pthread_mutex_lock_t : L.lltype =
    L.function_type i32_t [| double_ptr |] in
  let pthread_mutex_lock_func : L.llvalue =
    L.declare_function "pthread_mutex_lock" pthread_mutex_lock_t the_module in

  (*
   * https://linux.die.net/man/3/pthread_mutex_unlock
   *
   * pthread_mutex_unlock: C routine to unlock a mutex
   * 1st argument: Address of opaque struct pthread_mutex_t
   *)
  let pthread_mutex_unlock_t : L.lltype =
    L.function_type i32_t [| double_ptr |] in
  let pthread_mutex_unlock_func : L.llvalue =
    L.declare_function "pthread_mutex_unlock" pthread_mutex_unlock_t the_module in

  (*
   * C util functions
   *)
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  let sprintf_t : L.lltype =
    L.var_arg_function_type i32_t [| pointer_t; pointer_t |] in
  let sprintf_func : L.llvalue =
    L.declare_function "sprintf" sprintf_t the_module in


  let strlen_t : L.lltype =
    L.function_type i32_t [| L.pointer_type i8_t |] in
  let strlen_func : L.llvalue =
    L.declare_function "strlen" strlen_t the_module in

  let strcat_t : L.lltype =
    L.function_type (L.pointer_type i8_t) [| L.pointer_type i8_t; L.pointer_type i8_t |] in
  let strcat_func : L.llvalue =
    L.declare_function "strcat" strcat_t the_module in

  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder)

  (* Message queue implementation *)
  (*
   * Initialize a queue on the heap and return a pointer to it
   *
   * @param None
   * @return queue_ptr
   *)
  in let queue_init_func =
    let queue_init_t = L.function_type queue_ptr [| |] in
    let queue_init_func = L.define_function "Queue_init" queue_init_t the_module in

    let builder = L.builder_at_end context (L.entry_block queue_init_func) in
    let capacity = L.const_int i32_t 2 in
    let queue_alloca = L.build_alloca queue_ptr "queue_alloca" builder and
        queue_malloc = L.build_malloc queue_t "queue_malloc" builder and
        array_alloca = L.build_alloca data_double_ptr "data_alloca" builder and
        array_malloc = L.build_array_malloc data_ptr capacity "data_malloc" builder in

    let _ = L.build_store queue_malloc queue_alloca builder and
        _ = L.build_store array_malloc array_alloca builder in

    let queue = L.build_load queue_alloca "queue_load" builder and
        array = L.build_load array_alloca "array_load" builder in

    let { size = size_ptr; cap = cap_ptr; array = array_ptr } = build_queue_gep queue builder in
    let _ = L.build_store (L.const_int i32_t 0) size_ptr builder and
        _ = L.build_store capacity cap_ptr builder and
        _ = L.build_store array array_ptr builder and
        _ = add_terminal builder (L.build_ret queue) in queue_init_func

  (*
   * Returns true if queue is empty and otherwise false
   *
   * @param queue_ptr
   * @return bool_t
   *)
  in let queue_empty_func =
    let queue_empty_t = L.function_type i1_t [| queue_ptr |] in
    let queue_empty_func = L.define_function "Queue_empty" queue_empty_t the_module in

    let builder = L.builder_at_end context (L.entry_block queue_empty_func) in
    (* Extract queue_ptr *)
    let argument = L.param queue_empty_func 0 in
    let queue_alloca = L.build_alloca queue_ptr "queue_alloca" builder in
    let _ = L.build_store argument queue_alloca builder in

    let queue = L.build_load queue_alloca "queue_load" builder in
    let { size = size_ptr; _ } = build_queue_gep queue builder in
    let size = L.build_load size_ptr "size_load" builder in
    let empty = L.build_icmp L.Icmp.Eq size (L.const_int i32_t 0) "queue_empty" builder in
    let _ = add_terminal builder (L.build_ret empty) in queue_empty_func in

  (* WIP factor *)
  (* let queue_push_func =
    let queue_push_t = L.function_type void_t [| queue_ptr; data_ptr |] in
    let queue_push_func = L.define_function "Queue_push" queue_push_t the_module in

    let resize_bb = L.append_block context "queue_resize" queue_push_func and
        copy_bb = L.append_block context "queue_copy" queue_push_func and
        push_bb = L.append_block context "queue_push" queue_push_func in

    let builder = L.builder_at_end context (L.entry_block queue_push_func) and
        resize_builder = L.builder_at_end context resize_bb and
        copy_builder = L.builder_at_end context copy_bb and
        push_builder = L.builder_at_end context push_bb in

    let queue_alloca = L.build_alloca queue_ptr "queue_alloca" builder and
        array_alloca = L.build_alloca data_double_ptr "array_alloca" builder and
        data_alloca = L.build_alloca data_ptr "data_alloca" builder and
        index_alloca = L.build_alloca i32_t "index_alloca" builder in


    (* Store input arguments into local variables *)
    let _ = L.build_store (L.param queue_push_func 0) queue_alloca builder and
        _ = L.build_store (L.param queue_push_func 1) data_alloca builder and
        _ = L.build_store (L.const_int i32_t 0) index_alloca builder in

    (* Load local variables *)
    let queue = L.build_load queue_alloca "queue_load" builder and
        data = L.build_load data_alloca "data_load" builder in

    (* Get pointers to queue *)
    let { size = size_ptr; cap = cap_ptr; array = array_ptr } = build_queue_gep queue builder in
    (* Extract size and capacity *)
    let size = L.build_load size_ptr "size_load" builder and
        capacity = L.build_load cap_ptr "capacity_load" builder in
    (* Check if size == capacity *)
    let full = L.build_icmp L.Icmp.Eq size capacity "queue_full" builder in
    let _ = L.build_cond_br full resize_bb push_bb builder in

    (* Make a new array *)
    let new_capacity = L.build_mul capacity (L.const_int i32_t 2) "double_capacity" resize_builder in
    let new_array_malloc = L.build_array_malloc data_ptr new_capacity "new_array_malloc" resize_builder in
    let _ = L.build_store new_array_malloc array_alloca resize_builder in
    let _ = L.build_br copy_bb resize_builder in

    (* Copy data from old array to new array *)
    let index = L.build_load index_alloca "index_load" copy_builder in
    let old_array = L.build_load array_ptr "old_array_load" copy_builder in
    let new_array = L.build_load array_alloca "new_array_load" copy_builder in
    let old_data_ptr = L.build_in_bounds_gep old_array [| index |] "gep_old_data" copy_builder in
    let new_data_ptr = L.build_in_bounds_gep new_array [| index |] "gep_new_data" copy_builder in
    let old_data = L.build_load old_data_ptr "old_data_load" *)
  let queue_push_func =
    let queue_push_t = L.function_type void_t [| queue_ptr; data_ptr |] in
    let queue_push_func = L.define_function "Queue_push" queue_push_t the_module in

    let resize_bb = L.append_block context "resize_queue" queue_push_func and
        cond_bb = L.append_block context "copy_cond" queue_push_func and
        copy_bb = L.append_block context "copy_data" queue_push_func and
        post_copy_bb = L.append_block context "post_copy_data" queue_push_func and
        terminate_bb = L.append_block context "terminate_copy" queue_push_func and
        push_bb = L.append_block context "push_data" queue_push_func in

    let builder = L.builder_at_end context (L.entry_block queue_push_func) and
        resize_builder = L.builder_at_end context resize_bb and
        cond_builder = L.builder_at_end context cond_bb and
        copy_builder = L.builder_at_end context copy_bb and
        post_copy_builder = L.builder_at_end context post_copy_bb and
        terminate_builder = L.builder_at_end context terminate_bb and
        push_builder = L.builder_at_end context push_bb in

    let queue_alloca = L.build_alloca queue_ptr "queue_alloca" builder and
        data_array_alloca = L.build_alloca data_double_ptr "data_array_alloca" builder and
        data_alloca = L.build_alloca data_ptr "data_alloca" builder and
        index_alloca = L.build_alloca i32_t "index_alloca" builder in
    let _ = L.build_store (L.param queue_push_func 0) queue_alloca builder and
        _ = L.build_store (L.param queue_push_func 1) data_alloca builder and
        _ = L.build_store (L.const_int i32_t 0) index_alloca builder in
    let queue = L.build_load queue_alloca "queue_load" builder in
    let size_ptr = L.build_in_bounds_gep queue [| gep_index 0; gep_index 0 |] "gep_size" builder and
        capacity_ptr = L.build_in_bounds_gep queue [| gep_index 0; gep_index 1|] "gep_capacity" builder in
    let size = L.build_load size_ptr "size_load" builder and
        capacity = L.build_load capacity_ptr "capacity_load" builder in
    let full = L.build_icmp L.Icmp.Eq size capacity "queue_is_full" builder in
    let _ = L.build_cond_br full resize_bb push_bb builder in

    let new_capacity = L.build_mul capacity (L.const_int i32_t 2) "double_capacity" resize_builder in
    let data_array_malloc = L.build_array_malloc data_ptr new_capacity "data_array_malloc" resize_builder in
    let _ = L.build_store data_array_malloc data_array_alloca resize_builder in
    let _ = L.build_br cond_bb resize_builder in

    let index = L.build_load index_alloca "index_load" cond_builder in
    let finished = L.build_icmp L.Icmp.Eq index size "finished_copy" cond_builder in
    let _ = L.build_cond_br finished terminate_bb copy_bb cond_builder in

    let queue = L.build_load queue_alloca "queue_load" copy_builder in
    let old_data_array_ptr = L.build_in_bounds_gep queue [| gep_index 0; gep_index 2 |] "gep_old_data_array" copy_builder in
    let old_data_array = L.build_load old_data_array_ptr "old_data_array_load" copy_builder in
    let index = L.build_load index_alloca "index_load" copy_builder in
    let old_data_ptr = L.build_in_bounds_gep old_data_array [| index |] "gep_old_data" copy_builder in
    let data = L.build_load old_data_ptr "data_load" copy_builder in
    let new_data_array = L.build_load data_array_alloca "new_data_array_load" copy_builder in
    let new_data_ptr = L.build_in_bounds_gep new_data_array [| index |] "gep_new_data" copy_builder in
    let _ = L.build_store data new_data_ptr copy_builder in
    let _ = L.build_br post_copy_bb copy_builder in

    let index = L.build_load index_alloca "index_load" post_copy_builder in
    let increment = L.build_add index (L.const_int i32_t 1) "increment_index" post_copy_builder in
    let _ = L.build_store increment index_alloca post_copy_builder in
    let _ = L.build_br cond_bb post_copy_builder in

    let queue = L.build_load queue_alloca "queue_load" terminate_builder in
    let new_data_array = L.build_load data_array_alloca "new_data_array_load" terminate_builder in
    let old_data_array_ptr = L.build_in_bounds_gep queue [| gep_index 0; gep_index 2|] "gep_old_data_array" terminate_builder in
    let _ = L.build_store new_data_array old_data_array_ptr terminate_builder in
    let new_capacity = L.build_mul capacity (L.const_int i32_t 2) "double_capacity" terminate_builder in
    let _ = L.build_store new_capacity capacity_ptr terminate_builder in
    let _ = L.build_br push_bb terminate_builder in

    let queue = L.build_load queue_alloca "queue_load" push_builder and
        data = L.build_load data_alloca "data_load" push_builder in
    let data_array_ptr = L.build_in_bounds_gep queue [| gep_index 0; gep_index 2 |] "gep_data_array" push_builder in
    let data_array = L.build_load data_array_ptr "data_load" push_builder in
    let data_ptr = L.build_in_bounds_gep data_array [| size |] "gep_data_ptr" push_builder in
    let _ = L.build_store data data_ptr push_builder in
    let new_size = L.build_add size (L.const_int i32_t 1) "increment_size" push_builder in
    let _ = L.build_store new_size size_ptr push_builder in
    let _ = add_terminal push_builder L.build_ret_void in queue_push_func

  in let queue_pop_func =
    let queue_pop_t = L.function_type data_ptr [| queue_ptr |] in
    let queue_pop_func = L.define_function "Queue_pop" queue_pop_t the_module in

    let cond_bb = L.append_block context "copy_cond" queue_pop_func and
        copy_bb = L.append_block context "copy_data" queue_pop_func and
        post_copy_bb = L.append_block context "post_copy_data" queue_pop_func and
        pop_bb = L.append_block context "pop_data" queue_pop_func in

    let builder = L.builder_at_end context (L.entry_block queue_pop_func) and
        cond_builder = L.builder_at_end context cond_bb and
        copy_builder = L.builder_at_end context copy_bb and
        post_copy_builder = L.builder_at_end context post_copy_bb and
        pop_builder = L.builder_at_end context pop_bb in

    let queue_alloca = L.build_alloca queue_ptr "queue_alloca" builder and
        data_alloca = L.build_alloca data_ptr "data_alloca" builder and
        index_alloca = L.build_alloca i32_t "index_alloca" builder in

    let _ = L.build_store (L.param queue_pop_func 0) queue_alloca builder in
    let queue = L.build_load queue_alloca "queue_load" builder in
    let size_ptr = L.build_in_bounds_gep queue [| gep_index 0; gep_index 0 |] "gep_size" builder in
    let data_array_ptr = L.build_in_bounds_gep queue [| gep_index 0; gep_index 2 |] "gep_data_array" builder in
    let data_array = L.build_load data_array_ptr "data_array_load" builder in
    let data_ptr = L.build_in_bounds_gep data_array [| gep_index 0 |] "gep_data" builder in
    let data = L.build_load data_ptr "data_load" builder in

    let _ = L.build_store (L.const_int i32_t 1) index_alloca builder and
        _ = L.build_store data data_alloca builder and
        _ = L.build_br cond_bb builder in

    let index = L.build_load index_alloca "index_load" cond_builder in
    let size = L.build_load size_ptr "size_load" cond_builder in
    let has_data = L.build_icmp L.Icmp.Slt index size "can_copy" cond_builder in
    let _ = L.build_cond_br has_data copy_bb pop_bb cond_builder in

    let index = L.build_load index_alloca "index_load" copy_builder in
    let curr_data_ptr = L.build_in_bounds_gep data_array [| index |] "gep_curr_data" copy_builder in
    let prev_index = L.build_sub index (L.const_int i32_t 1) "prev_index" copy_builder in
    let prev_data_ptr = L.build_in_bounds_gep data_array [| prev_index |] "gep_prev_data" copy_builder in
    let curr_data = L.build_load curr_data_ptr "data_load" copy_builder in
    let _ = L.build_store curr_data prev_data_ptr copy_builder in
    let _ = L.build_br post_copy_bb copy_builder in

    let index = L.build_load index_alloca "index_load" post_copy_builder in
    let increment_index = L.build_add index (L.const_int i32_t 1) "increment_index" post_copy_builder in
    let _ = L.build_store increment_index index_alloca post_copy_builder in
    let _ = L.build_br cond_bb post_copy_builder in

    let data = L.build_load data_alloca "data_load" pop_builder in
    let size = L.build_load size_ptr "size_load" pop_builder in
    let new_size = L.build_sub size (L.const_int i32_t 1) "decrement_size" pop_builder in
    let _ = L.build_store new_size size_ptr pop_builder in
    let _ = add_terminal pop_builder (L.build_ret data) in queue_pop_func

  (*
   * Takes a boolean value and return string true or false
   *
   * @param bool
   * @return string_ptr
   *)
  in let string_of_bool_func =
    let string_of_bool_t = L.function_type pointer_t [| i1_t |] in
    let string_of_bool_func = L.define_function "string_of_bool" string_of_bool_t the_module in
    let argument = L.param string_of_bool_func 0 in
    let true_bb = L.append_block context "true_bb" string_of_bool_func and
        false_bb = L.append_block context "false_bb" string_of_bool_func in
    let builder = L.builder_at_end context (L.entry_block string_of_bool_func) and
        true_builder = L.builder_at_end context true_bb and
        false_builder = L.builder_at_end context false_bb in
    let true_value = L.build_global_stringptr "true" "#t" builder and
        false_value = L.build_global_stringptr "false" "#f" builder in
    let _ = L.build_cond_br argument true_bb false_bb builder in
    let _ = add_terminal true_builder (L.build_ret true_value) and
        _ =  add_terminal false_builder (L.build_ret false_value) in string_of_bool_func

  in let string_equiv_func =
    let string_equiv_func = L.function_type i1_t [| pointer_t; pointer_t |] in
    let string_equiv_func = L.define_function "string_equiv" string_equiv_func the_module in
    let x = L.param string_equiv_func 0 and
        y = L.param string_equiv_func 1 in
    let builder = L.builder_at_end context (L.entry_block string_equiv_func) in
    let counter = L.build_alloca i32_t "counter" builder in
    let _ = L.build_store (L.const_int i32_t 0) counter builder in
    let x_char_alloca = L.build_alloca i8_t "x_char_alloca" builder and
        y_char_alloca = L.build_alloca i8_t "y_char_alloca" builder in
    let pred_bb = L.append_block context "pred_bb" string_equiv_func in
    let false_bb = L.append_block context "false_bb" string_equiv_func in
    let true_bb = L.append_block context "true_bb" string_equiv_func in
    let continue_bb = L.append_block context "continue_bb" string_equiv_func in
    let _ = L.build_br pred_bb builder in

    let pred_builder = L.builder_at_end context pred_bb in
    let counter_load = L.build_load counter "counter_load" pred_builder in
    let x_char = L.build_in_bounds_gep x [|counter_load|] "gep_x_char" pred_builder in
    let y_char = L.build_in_bounds_gep y [|counter_load|] "gep_y_char" pred_builder in
    let x_char_load = L.build_load x_char "x_char_load" pred_builder in
    let y_char_load = L.build_load y_char "y_char_load" pred_builder in
    let _ = L.build_store x_char_load x_char_alloca pred_builder in
    let _ = L.build_store y_char_load y_char_alloca pred_builder in
    let increment = L.build_add counter_load (L.const_int i32_t 1) "increment_index" pred_builder in
    let _ = L.build_store increment counter pred_builder in
    let pred = L.build_icmp L.Icmp.Eq x_char_load y_char_load "pred" pred_builder in
    let _ = L.build_cond_br pred continue_bb false_bb pred_builder in

    let false_builder = L.builder_at_end context false_bb in
    let _ = L.build_ret (L.const_int i1_t 0) false_builder in

    let continue_builder = L.builder_at_end context continue_bb in
    let continue_pred = (L.build_icmp L.Icmp.Eq (L.build_load x_char_alloca "x_char_load" continue_builder)
                        (L.const_int i8_t 0) "x_char_neq_null" continue_builder) in
    let _ = L.build_cond_br continue_pred true_bb pred_bb continue_builder in

    let true_builder = L.builder_at_end context true_bb in
    let _ = L.build_ret (L.const_int i1_t 1) true_builder in
    string_equiv_func



  (* Build instructions to concatenate two strings. Matches call signature of functions like L.build_add *)
  in let build_strcat x y name builder =
    let xlength = L.build_call strlen_func [| x |] "strlen_x" builder and
        ylength = L.build_call strlen_func [| y |] "strlen_y" builder in
    (* let _ = L.build_call printf_func [| L.build_global_stringptr "%d\n" "fmt" builder ; xlength |] "print x length" builder *)
    let combined_length = L.build_add xlength ylength "new_length" builder in
    let combined_length_with_null = L.build_add combined_length (L.const_int i32_t 1) "new_length" builder in
    (* Allocate space for our concatenated string *)
    let new_string = L.build_array_alloca i8_t combined_length_with_null "allocated_combined_string" builder in
    (* Set the first place in our new string to the null characters *)
    let first_element = L.build_gep new_string [| (L.const_int i8_t 0) |] "first_element" builder in
    let _ = L.build_store (L.const_int i8_t 0) first_element builder in
    (* Call strcat for x *)
    let _ = L.build_call strcat_func [| new_string; x |] "strcat_x" builder in
    (* Call strcat for y *)
    L.build_call strcat_func [| new_string; y |] name builder in

  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
	      Array.of_list (List.map (fun (t,_) -> lltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (lltype_of_typ fdecl.sret_type) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty fdecls in

  (* TODO - Extend map with extra information? For example, message passing? *)
  let thread_decls =
    let thread_decl m tdecl =
      let stname = tdecl.stname and
          ttype = L.function_type (L.pointer_type i8_t) [| L.pointer_type i8_t |] in
      (* Add paramaters for threads *)
      StringMap.add stname (L.define_function tdecl.stname ttype the_module, tdecl) m in
      (* StringMap.add stname (L.define_function (if tdecl.stname = "Main" then "main" else tdecl.stname) ttype the_module, tdecl) m in *)
    List.fold_left thread_decl StringMap.empty tdecls in

  (* TODO - What is this for? *)
  (* let (the_thread, _) = StringMap.find "Main" thread_decls in
  let builder = L.builder_at_end context (L.entry_block the_thread) in
  let string_format_str = L.build_global_stringptr "%s" "fmt" builder in
  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in *)

  let build_body ?(arg_gep : arg_gep option) (builder, env) sstmt the_thread =
    let string_format_str = L.build_global_stringptr "%s" "fmt" builder in
    let int_format_str = L.build_global_stringptr "%d" "fmt" builder in
    let float_format_str = L.build_global_stringptr "%f" "fmt" builder in
    let pthread_ts = ref [] in

    let rec expr ((builder: L.llbuilder), env) ((typ, sexpr : sexpr)) =
      match sexpr with
        | SIntLit i -> (L.const_int i32_t i, env)
        | SStringLit s -> (L.build_global_stringptr s "tmp" builder, env)
        | SBoolLit b -> (L.const_int i1_t (if b then 1 else 0), env)
        | SFloatLit l -> (L.const_float_of_string float_t l, env)
        | (STupleLit _) as sexpr  ->
            let rec build_tuple ptr builder (sexpr : sexpr) =
              let { tag = tag_ptr; head = head_ptr; tail = tail_ptr } = build_data_gep ptr builder in
                match sexpr with
                  (ty, STupleLit (fst, snd)) ->
                    let tag_value = tag_of_type ty in
                    let head_malloc = L.build_malloc data_t "head_malloc" builder and
                        head_alloca = L.build_alloca data_ptr "head_alloca" builder and
                        tail_malloc = L.build_malloc data_t "head_malloc" builder and
                        tail_alloca = L.build_alloca data_ptr "head_alloca" builder in

                    let _ = L.build_store head_malloc head_alloca builder and
                        _ = L.build_store tail_malloc tail_alloca builder and
                        _ = L.build_store tag_value tag_ptr builder in

                    let head_load = L.build_load head_alloca "head_load" builder in
                    let tail_load = L.build_load tail_alloca "tail_load" builder in
                    let head_cast = cast_llvalue_to_ptr ty head_load builder in
                    let tail_cast = cast_llvalue_to_ptr ty tail_load builder in
                    let _ = L.build_store head_cast head_ptr builder in
                    let _ = L.build_store tail_cast tail_ptr builder in
                    let _ = build_tuple head_load builder fst in
                    let _ = build_tuple tail_load builder snd in
                    ()
                | (ty, _) ->
                  let tag_value = tag_of_type ty in
                  let head_malloc = L.build_malloc (lltype_of_typ ty) "head_malloc" builder and
                      head_alloca = L.build_alloca (L.pointer_type (lltype_of_typ ty)) "head_alloca" builder in
                  let (llvalue, _) = expr (builder, env) sexpr in
                  let _ = L.build_store tag_value tag_ptr builder in
                  let _ = L.build_store llvalue head_malloc builder in
                  let _ = L.build_store head_malloc head_alloca builder in
                  let head_load = L.build_load head_alloca "head_load" builder in
                  let head_cast = cast_llvalue_to_ptr ty head_load builder in
                  let _ = L.build_store head_cast head_ptr builder in ()
            in
            let data_malloc = L.build_malloc data_t "data_malloc" builder in
            let data_alloca = L.build_alloca data_ptr "data_alloca" builder in
            let _ = L.build_store data_malloc data_alloca builder in
            let data = L.build_load data_alloca "data_load" builder in
            let _ = build_tuple data builder (typ, sexpr) in (data, env)
        | SArrayLit sexpr_list -> raise (Failure "array not implemented")
        | SNoexpr -> (L.const_int i32_t 0, env)
        | SId s -> (L.build_load (StringMap.find s env) s builder, env)
        | SCall ("print", [sexpr]) ->
            let (llvalue, env') =  expr (builder, env) sexpr in
            (L.build_call printf_func [| string_format_str; llvalue |] "printf" builder, env')
        | SCall ("string_of_bool", [sexpr]) ->
            let (llvalue, env') = expr (builder, env) sexpr in
            (L.build_call string_of_bool_func [| llvalue |] "string_of_bool" builder, env')
        | SCall ("string_of_int", [sexpr]) ->
            let (llvalue, env') = expr (builder, env) sexpr in
            let buf_malloc = L.build_array_malloc i8_t (L.const_int i32_t 10) "buf_malloc" builder in
            let buf_alloca = L.build_alloca (L.pointer_type i8_t) "buf_alloca" builder in
            let _ = L.build_store buf_malloc buf_alloca builder in
            let buf = L.build_load buf_alloca "buf_load" builder in
            let _ = (L.build_call sprintf_func [| buf ; int_format_str; llvalue |]
                                                "string_of_int" builder)
            in (buf, env')
        | SCall ("string_of_float", [sexpr]) ->
            let (llvalue, env') = expr (builder, env) sexpr in
            let buf_malloc = L.build_array_malloc i8_t (L.const_int i32_t 10) "buf_malloc" builder in
            let buf_alloca = L.build_alloca (L.pointer_type i8_t) "buf_alloca" builder in
            let _ = L.build_store buf_malloc buf_alloca builder in
            let buf = L.build_load buf_alloca "buf_load" builder in
            let _ = (L.build_call sprintf_func [| buf ; float_format_str; llvalue |]
                                                "string_of_float" builder)
            in (buf, env')
        | SCall (func_name, arg_list) ->
            let (fdef, fdecl) = StringMap.find func_name function_decls in
            let llargs = List.map (fun e -> let (llvalue, _) = expr (builder, env) e in llvalue) arg_list in
            let result = (match fdecl.sret_type with
              A.Void -> ""
              | _ -> func_name ^ "_result") in
            (L.build_call fdef (Array.of_list llargs) result builder, env)
        | SBinop (e1, op, e2) ->
            let (t1, _) = e1 in
            let (e1', env') = expr (builder, env) e1 in
            let (e2', env'') = expr (builder, env') e2 in
            let op =
              (match t1 with
                A.Float ->
                  (match op with
                      A.Add              -> L.build_fadd
                    | A.Sub              -> L.build_fsub
                    | A.Mult             -> L.build_fmul
                    | A.Div              -> L.build_fdiv
                    | A.Equality         -> L.build_fcmp L.Fcmp.Oeq
                    | A.Pow              -> raise (Failure "Implement power on float")
                    | A.Neq              -> L.build_fcmp L.Fcmp.One
                    | A.Less             -> L.build_fcmp L.Fcmp.Olt
                    | A.Leq              -> L.build_fcmp L.Fcmp.Ole
                    | A.Greater          -> L.build_fcmp L.Fcmp.Ogt
                    | A.Geq              -> L.build_fcmp L.Fcmp.Oge
                    | A.And | A.Or | Mod ->
                        raise (Failure "internal error: semant should have rejected
                                        and/or/mod on float"))
                | A.Int ->
                  (match op with
                    | A.Add       -> L.build_add
                    | A.Sub       -> L.build_sub
                    | A.Mult      -> L.build_mul
                    | A.Div       -> L.build_sdiv
                    | A.And       -> L.build_and
                    | A.Mod       -> L.build_srem
                    | A.Or        -> L.build_or
                    | A.Equality  -> L.build_icmp L.Icmp.Eq
                    | A.Neq       -> L.build_icmp L.Icmp.Ne
                    | A.Less      -> L.build_icmp L.Icmp.Slt
                    | A.Leq       -> L.build_icmp L.Icmp.Sle
                    | A.Greater   -> L.build_icmp L.Icmp.Sgt
                    | A.Geq       -> L.build_icmp L.Icmp.Sge
                    | A.Pow       -> raise (Failure "Implement power on int"))
                | A.String ->
                  (match op with
                    | A.Add -> build_strcat
                    | A.Equality ->
                      (fun e1' e2' var_name builder -> (L.build_call string_equiv_func [| e1'; e2' |] var_name builder))
                    | A.Neq ->
                      (fun e1' e2' var_name builder ->
                        L.build_icmp L.Icmp.Eq (L.const_int i1_t 0)
                          (L.build_call string_equiv_func [| e1'; e2' |] var_name builder)
                          var_name
                          builder)
                    | _ -> raise (Failure "Operation not supported on string arguments"))
                | _ -> raise (Failure "Implement other")) in
            (op e1' e2' "binop_result" builder, env'')
        | SUnop (unop, expr) -> raise (Failure "unop not implemented")
        | SIndex (arr_name, index) -> raise (Failure "index not implemented")
        | SUnit -> raise (Failure "sunit not implemented")
        | SSpawn tn ->
          (*
           * Generate child queue
           * Store parent, child into routine_t
           * Call pthread_create
           * Expression return child queue
           *)
          let { child_queue = self_queue_ptr; _ } = (match arg_gep with
            (* @TODO - Come back later *)
              None -> raise (Failure "Spawn not allowed inside function")
            | Some arg_gep -> arg_gep) in

          let (thread, _) = StringMap.find tn thread_decls in
          (* Allocate &pthread_t *)
          let id = L.build_alloca pointer_t "id" builder in
          let arg_malloc = L.build_malloc arg_t "arg_malloc" builder and
              arg_alloca = L.build_alloca arg_ptr "arg_alloca" builder and
              child_queue_alloca = L.build_alloca queue_ptr "child_queue_alloc" builder and
              child_queue = L.build_call queue_init_func [| |] "child_queue_init" builder in

          let _ = L.build_store arg_malloc arg_alloca builder and
              _ = L.build_store child_queue child_queue_alloca builder in

          let arg = L.build_load arg_alloca "arg_load" builder and
              child_queue = L.build_load child_queue_alloca "child_queue_load" builder in
          let { parent_queue = parent_queue_ptr; child_queue = child_queue_ptr; _ }
            = build_arg_gep arg builder in

          let current_queue = L.build_load self_queue_ptr "self_queue_load" builder in
          let _ = L.build_store current_queue parent_queue_ptr builder and
              _ =  L.build_store child_queue child_queue_ptr builder in

          let arg = L.build_bitcast arg pointer_t "cast_arg" builder in
          let _ = L.build_call pthread_create_func [| id; (L.const_null i8_t); thread; arg|] "create" builder in
          pthread_ts := id :: !pthread_ts;
          (child_queue, env);
        | SAssign (var_name, v) ->
            let (value_to_assign, env') = expr (builder, env) v in
            let storage = StringMap.find var_name env' in
            let _ = L.build_store value_to_assign storage builder in
            (value_to_assign, env')
        | SAssignIndex (var_name, index, value) -> raise (Failure "Implement SAssignIndex")
        | _ -> raise (Failure "TODO")
    and stmt ((builder: L.llbuilder), env) sstmt =
      match sstmt with
          SBlock sblock ->
            let (builder_final, _) = List.fold_left stmt (builder, env) sblock in
            (builder_final, env)
        | SExpr sexpr -> let (_, env2) = expr (builder, env) sexpr in (builder, env2)
        | SReturn sexpr -> let (value, _) = expr (builder, env) sexpr in
            let _ = L.build_ret value builder in
            (builder, env)
        | SBreak -> raise (Failure "implement break")
        | SContinue -> raise (Failure "implement continue")
        | SDecl (ty, var_name, sx) ->
            let (llvalue, env2) = expr (builder, env) sx in
            let alloca = L.build_alloca (match ty with
              | Void -> void_t
              | Bool -> i1_t
              | Int -> i32_t
              | Float -> float_t
              | String -> pointer_t
              | Thread -> queue_ptr
              | Semaphore -> void_t
              | Tuple (t1, t2) -> data_ptr
              | Array (arrayType, count) -> raise (Failure "TODO array")) var_name builder in
            let _ = L.build_store llvalue alloca builder in
            (builder, StringMap.add var_name alloca env2)
        | SFor (init_stmt, pred_expr, iter_expr, body_stmt) -> raise (Failure "implement sfor")
        | SWhile (pred_expr, body) ->
            let pred_bb = L.append_block context "while_pred" the_thread in
            let body_bb = L.append_block context "while_body" the_thread in
            let _ = L.build_br pred_bb builder in
            let pred_builder = L.builder_at_end context pred_bb in
            let (pred, _) = expr (pred_builder, env) pred_expr in
            let end_bb = (L.append_block context "while_end" the_thread) in
            let end_builder = L.builder_at_end context end_bb in
            let _ = L.build_cond_br pred body_bb end_bb pred_builder in
            let body_builder = L.builder_at_end context body_bb in
            let (final_builder, _) = stmt (body_builder, env) body in
            let _ = L.build_br pred_bb final_builder in
            (end_builder, env)
        | SIf (pred_expr, t_block, f_block) ->
            let t_bb = L.append_block context "if_t" the_thread in
            let t_builder = L.builder_at_end context t_bb in
            let f_bb = L.append_block context "if_f" the_thread in
            let f_builder = L.builder_at_end context f_bb in
            let end_bb = L.append_block context "if_end" the_thread in
            let end_builder = L.builder_at_end context end_bb in
            let (pred, _) = expr (builder, env) pred_expr in
            let _ = L.build_cond_br pred t_bb f_bb builder in
            let (t_builder, _) = stmt (t_builder, env) t_block in
            let _ = L.build_br end_bb t_builder in
            let (f_builder, _) = stmt (f_builder, env) f_block in
            let _ = L.build_br end_bb f_builder in
            (end_builder, env)

        | SSend (receiver_name, sexpr) ->
            let (typ, _) = sexpr in
            let receiver_queue_ptr = StringMap.find receiver_name env in
            let receiver_queue = L.build_load receiver_queue_ptr "receiver_queue_load" builder in
            let (llvalue, env') = expr (builder, env) sexpr in
            (* @TODO - Change this code after we implement tuple *)
            (match typ with
                A.Tuple _ ->
                  let _ = L.build_call queue_push_func [| receiver_queue; llvalue |] "" builder in
                  (builder, env')
              | A.Array _ -> raise (Failure "TODO Array send")
              | _ ->
                let data_alloca = L.build_alloca data_ptr "data_alloca" builder and
                    data_malloc = L.build_malloc data_t "data_malloc" builder and
                    head_alloca = L.build_alloca (L.pointer_type (lltype_of_typ typ)) "head_alloca" builder and
                    (* head_alloca = L.build_alloca (lltype_of_typ typ) "head_alloca" builder and *)
                    head_malloc = L.build_malloc (lltype_of_typ typ) "head_malloc" builder and
                    tail_alloca = L.build_alloca (L.pointer_type (lltype_of_typ typ)) "tail_alloca" builder and
                    tail_malloc = L.build_malloc (lltype_of_typ typ) "tail_malloc" builder in

                let _ = L.build_store data_malloc data_alloca builder and
                    _ = L.build_store head_malloc head_alloca builder and
                    _ = L.build_store tail_malloc tail_alloca builder in
                    (* _ = L.build_store head_malloc head_alloca builder and
                    _ = L.build_store tail_malloc tail_alloca builder in *)

                let data = L.build_load data_alloca "data_load" builder in
                let { tag = tag_ptr; head = head_ptr; tail = tail_ptr } =
                  build_data_gep data builder in

                let tag_value = tag_of_type typ in
                let _ = L.build_store llvalue head_malloc builder in
                let _ = L.build_store (L.const_null (L.pointer_type (lltype_of_typ typ))) tail_alloca builder in

                let _ = L.build_store tag_value tag_ptr builder in
                let head_load = L.build_load head_alloca "head_load" builder in
                let head_cast = cast_llvalue_to_ptr typ head_load builder in
                let tail_load = L.build_load tail_alloca "tail_load" builder in
                let tail_cast = cast_llvalue_to_ptr typ tail_load builder in
                let _ = L.build_store head_cast head_ptr builder in
                let _ = L.build_store tail_cast tail_ptr builder in
                (* let head_cast = cast_llvalue_to_ptr typ head_malloc builder in
                let tail_cast = cast_llvalue_to_ptr typ tail_malloc builder in
                let tag_value = tag_of_type typ in
                let _ = L.build_store tag_value tag_ptr builder in
                let _ = L.build_store llvalue head_malloc builder in
                let _ = L.build_store head_cast head_ptr builder in
                let _ = L.build_store tail_cast tail_ptr builder in *)

                let _ = L.build_call queue_push_func [| receiver_queue; data |] "" builder in
                (builder, env'))
        | SSendParent (message_expr) -> raise (Failure "todo")
          (* let parent_pool = (match parent_pool with
           | None -> raise (Failure "Cannot send message to parent thread pool from inside a function")
           | Some thread_pool -> thread_pool) in
          let data_alloca = L.build_alloca data_ptr "data_alloca" builder and
              data_malloc = L.build_malloc data_t "data_malloc" builder and
              value_alloca = L.build_alloca i32_t "value_alloca" builder in
          let _ = L.build_store data_malloc data_alloca builder in
          let _ = L.build_store (L.const_int i32_t 100) value_alloca builder in
          let data = L.build_load data_alloca "data_load" builder in
          let head_ptr = L.build_in_bounds_gep data [| gep_index 0; gep_index 1 |] "gep_head" builder in
          let value = L.build_load value_alloca "value_load" builder in
          let cast = L.build_inttoptr value pointer_t "value_cast" builder in
          let _ = L.build_store cast head_ptr builder in
          let _ = L.build_call queue_push_func [| parent_pool; data |] "" builder in *)
          (* let data_malloc = L.build_malloc data_t "data_malloc" builder and
              data_alloca = L.build_alloca data_ptr "data_alloca" builder and
              value_alloca = L.build_alloca i32_t "value_alloca" builder in
          let _ = L.build_store data_malloc data_alloca builder in
          let _ = L.build_store (L.const_int i32_t 10) value_alloca builder in

          let data = L.build_load data_alloca "data_load" builder in
          let value = L.build_load value_alloca "value_load" builder in
          let value = L.build_inttoptr value pointer_t "cast" builder in
          let data_ptr = L.build_in_bounds_gep data [| gep_index 0; gep_index 1 |] "gep_data" builder in
          let _ = L.build_store value data_ptr builder in
          let _ = L.build_call queue_push_func [| parent_pool; data |] "" builder in *)

          (* let value = L.build_load data_ptr "data_load" builder in
          let value = L.build_ptrtoint value i32_t "value" builder in
          let _ = L.build_call printf_func [| int_format_str; value |] "print_test" builder in *)

          (* (builder, env) *)
        | SReceive receive_cases ->
          (*
           * Build basic blocks for each pattern statement
           * For each pattern, besides the wcard, create an array of tag values
           * Store the array of tag values in an array of size n - 1
           * Loop through each array, compare data_t with the array pattern
           * If match, jump to that array basic block
           * Otherwise, jump to the wildcard basic block
           *)

          let { child_queue = self_queue_ptr; _ } = (match arg_gep with
              Some queue -> queue
            | None -> raise (Failure "Cannot receive message inside a function")) in
          (* let self_queue_alloca = L.build_alloca queue_ptr "self_queue_alloca" builder in *)
          let self_queue = L.build_load self_queue_ptr "self_queue_load" builder in
          (* let _ = L.build_store self_queue self_queue_alloca builder in
          let self_queue = L.build_load self_queue_alloca "self_queue_load" builder in *)

          let pred_bb = L.append_block context "pred" the_thread in
          let receive_bb = L.append_block context "receive" the_thread in
          let pred_builder = L.builder_at_end context pred_bb in
          let receive_builder = L.builder_at_end context receive_bb in

          (* Jump to the predicate builder *)
          let _ = L.build_br pred_bb pred_builder in
          let empty = L.build_call queue_empty_func [| self_queue |] "queue_empty" pred_builder in
          let _ = L.build_cond_br empty pred_bb receive_bb pred_builder in

          let data_alloca = L.build_alloca data_ptr "data_alloca" receive_builder and
              tag_alloca = L.build_alloca i32_t "tag_alloca" receive_builder and
              head_alloca = L.build_alloca pointer_t "head_alloca" receive_builder and
              tail_alloca = L.build_alloca pointer_t "tail_alloca" receive_builder in
          let data_pop = L.build_call queue_pop_func [| self_queue |] "queue_pop" receive_builder in
          let _ = L.build_store data_pop data_alloca receive_builder in

          (* and
              post_receive_bb = L.append_block context "post_receive" the_thread in

          let pred_builder = L.builder_at_end context pred_bb and
              post_receive_builder = L.builder_at_end context post_receive_bb in

            | Some arg_gep -> arg_gep ) in
          let array_tags_alloca =
            L.build_array_alloca
              (L.pointer_type i32_t)
              (L.const_int i32_t (List.length receive_cases))
              "array_tags" builder in

          let _ = L.build_br  *)
          (*
           * Takes a pattern, build the basic block, and return a basic block and a pattern
           *)
          let build_case_body index (pattern, sstmt) =
            let ocaml_tags = tag_pattern pattern in
            let tags_alloca = L.build_array_alloca i32_t (L.const_int i32_t (List.length ocaml_tags)) "tags_alloca" builder in
            let _ = List.iteri (fun index value ->
              let index = L.const_int i32_t index in
              let lltag = L.const_int 32_t value in
              let tags_index_ptr = L.build_in_bounds_gep tags_alloca [| index |] "gep_tag_index" builder
              in ignore (L.build_store lltag tags_index_ptr builder)) ocaml_tags in
            let arr_tags_ptr = L.build_in_bounds_gep tags_alloca [| L.const_int i32_t index |] "gep_tag_index" builder in
            let tags = L.build_load tags_alloca "tags_load" builder in
            let _ = L.build_store tags arr_tags_ptr builder in
            match pattern with
              SBasePattern (typ, id) ->
                let tag_num = tag_of_type typ in
                let case_bb = L.append_block context (A.string_of_typ typ) the_thread in
                let case_builder = L.builder_at_end context case_bb in
                (* @TODO - Ask about width of floats + pointers / potentially overflow *)
                let value_alloca = L.build_alloca (lltype_of_typ typ) "value_alloca" case_builder in
                let value_ptr = L.build_load head_ptr "value_ptr_load" case_builder in
                let value_ptr = L.build_bitcast value_ptr (L.pointer_type (lltype_of_typ typ)) "value_ptr_cast" case_builder in
                let value = L.build_load value_ptr "value_load" case_builder in
                let _ = L.build_store value value_alloca case_builder in
                let env' = StringMap.add id value_alloca env in
                let (new_builder, _) = stmt (case_builder, env') sstmt in
                let _ = L.build_br post_receive_bb new_builder in case_bb
            | STuplePattern _ ->
                let case_bb = L.append_block context "tuple_case" the_thread in
                let case_builder = L.builder_at_end context case_bb in
                let (new_builder, _) = stmt (case_builder, env) sstmt in
                let _ = L.build_br post_receive_bb new_builder in case_bb
            | SWildcardPattern ->
                let default_bb = L.append_block context "wildcard" the_thread in
                let default_builder = L.builder_at_end context default_bb in
                let (new_builder, _) = stmt (default_builder, env) sstmt in
                let _ = L.build_br post_receive_bb new_builder in default_bb
          (* let wildcard = List.find (fun (pattern, _) -> pattern = WildcardPattern) receive_cases in *)



          (*let
              receive_bb = L.append_block context "receive" the_thread and
              post_bb = L.append_block context "post_receive" the_thread and
              default_bb = L.append_block context "switch_end" the_thread in

          let pred_builder = L.builder_at_end context pred_bb and
              receive_builder = L.builder_at_end context receive_bb and
              post_builder = L.builder_at_end context post_bb and
              default_builder = L.builder_at_end context default_bb in

          (* Move the current builder to pred_bb *)
          let _ = L.build_br pred_bb builder in

          (* Pop data from the queue and pattern match *)


          let data = L.build_load data_alloca "data_load" receive_builder in
          let { tag = tag_ptr; head = head_ptr; tail = tail_ptr }
            = build_data_gep data receive_builder in
          let tag_value = L.build_load tag_ptr "tag_load" receive_builder and
              head_value = L.build_load head_ptr "head_load" receive_builder and
              tail_value = L.build_load tail_ptr "tail_load" receive_builder in
          let _ = L.build_store tag_value tag_alloca receive_builder and
              _ = L.build_store head_value head_alloca receive_builder and
              _ = L.build_store tail_value tail_alloca receive_builder in

          let switch = L.build_switch tag_value default_bb (List.length receive_cases) receive_builder in

          let build_case (pattern, sstmt) = match pattern with
              SBasePattern (typ, id) ->
            | SWildcardPattern ->
            | _ -> raise (Failure "Implement composite case") in

          let _ = List.iter build_case receive_cases in (post_builder, env) *)
    in
    let (builder, env') = stmt (builder, env) sstmt in
    let join pthread =
      let id = L.build_load pthread "pthread_t" builder in
      ignore (L.build_call pthread_join_func [| id; (L.const_null i8_t) |] "join" builder) in
    let _ = List.iter join !pthread_ts in (builder, env') in

  let build_thread_body tdecl =
    let (the_thread, _) = StringMap.find tdecl.stname thread_decls in
    let builder = L.builder_at_end context (L.entry_block the_thread) in
    let argument = L.build_bitcast (L.param the_thread 0) arg_ptr "cast_void" builder in
    let arg_gep = build_arg_gep argument builder in
    let (final_builder, _) =
      build_body ~arg_gep:arg_gep (builder, StringMap.empty) (SBlock tdecl.sbody) the_thread
    (* thread function follows pthread function type and returns a NULL pointer *)
    in add_terminal final_builder (L.build_ret (L.const_null pointer_t))
  and build_func_body fdecl =
    let (the_func, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_func) in
    let _ = build_body (builder, StringMap.empty) (SBlock fdecl.sbody) the_func in
    () in
    (* (L.build_ret (L.const_null (L.pointer_type i8_t)) final_builder) *)
  let _ = List.map build_thread_body tdecls in
  let _ = List.map build_func_body fdecls in

  let main_t = L.function_type i32_t [| |] in
  let main_func = L.define_function "main" main_t the_module in
    let builder = L.builder_at_end context (L.entry_block main_func) in
    let (main_thread, _) = StringMap.find "Main" thread_decls in
    let parent_mutex_alloca = L.build_alloca pointer_t "parent_pool_mutex_ptr" builder and
        child_mutex_alloca = L.build_alloca pointer_t "child_pool_mutex_ptr" builder and
        arg_malloc = L.build_malloc arg_t "arg_malloc" builder and
        arg_alloca = L.build_alloca arg_ptr "arg_alloca" builder and
        parent_pool_alloca = L.build_alloca queue_ptr "parent_pool" builder and
        child_pool_alloca = L.build_alloca queue_ptr "child_pool" builder and
        parent_pool = L.build_call queue_init_func [| |] "init_parent_pool" builder and
        child_pool = L.build_call queue_init_func [| |] "init_child_pool" builder in
    let _ = L.build_store parent_pool parent_pool_alloca builder in
    let _ = L.build_store child_pool child_pool_alloca builder in
    let _ = L.build_store arg_malloc arg_alloca builder in

    (* TODO - mutex_init is for some reason giving segfault *)
    let arg = L.build_load arg_alloca "arg_load" builder in
    let parent_pool_ptr = L.build_in_bounds_gep arg [| gep_index 0; gep_index 2 |] "grep_parent_pool" builder in
    let child_pool_ptr = L.build_in_bounds_gep arg [| gep_index 0; gep_index 3 |] "grep_child_pool" builder in
    let _ = L.build_store parent_pool parent_pool_ptr builder in
    let _ = L.build_store child_pool child_pool_ptr builder in

    let arg = L.build_bitcast arg pointer_t "cast_arg" builder in
    let _ = L.build_call main_thread [| arg |] "start_main_thread" builder in
    let _ = add_terminal builder (L.build_ret (L.const_int i32_t 0)) in the_module
