module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

type sem_gep = {
  mutex : L.llvalue;
  count : L.llvalue;
}

type array_gep = {
  size : L.llvalue;
  data_array : L.llvalue;
  tags : L.llvalue;
  tags_size : L.llvalue;
}

type queue_gep = {
  size : L.llvalue;
  cap : L.llvalue;
  array : L.llvalue;
  mutex : L.llvalue;
}

type data_gep = {
  tag : L.llvalue;
  head : L.llvalue;
  tail : L.llvalue;
}

type arg_gep = {
  parent_queue : L.llvalue;
  child_queue : L.llvalue;
}

(* for implementing break / continue *)
type stmt_context = {
  continue_target_block : L.llbasicblock option;
  break_target_block : L.llbasicblock option;
}


let deep_copy_stringmap map =
  let new_map = StringMap.empty in
  let new_map = StringMap.fold (fun k v acc -> StringMap.add k v acc) map new_map in
  new_map

let translate ((tdecls : sthread_decl list), (fdecls : sfunc_decl list)) =
  let context = L.global_context () in
  let i32_t      = L.i32_type    context
  and i64_t      = L.i64_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type context in
  let pointer_t = L.pointer_type i8_t in
  let double_ptr = L.pointer_type pointer_t in
  let the_module = L.create_module context "Untangled" in

  let lltype_of_primitive = function
      A.Int   -> i64_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.String -> pointer_t
    | _ -> raise (Failure "Not a primitive type")
  in
  (* For array type in Untangled *)
  let array_t = L.struct_type context [| i32_t; pointer_t; L.pointer_type i32_t; i32_t |] in
  let array_ptr = L.pointer_type array_t in

  (*
   * Semaphore representation
   * @field 1 pthread_mutex_t*
   * @field 2 Count value
   *)
  let sem_t = L.struct_type context [| (lltype_of_primitive Int); pointer_t |] in
  let sem_ptr = L.pointer_type sem_t in

  let data_t = L.struct_type context [| i32_t; pointer_t; pointer_t |] in
  let data_ptr = L.pointer_type data_t in
  let data_double_ptr = L.pointer_type data_ptr in
  (*
   * struct Queue_t
   * @field 1 size of queue
   * @field 2 capacity of queue
   * @field 3 pointer to an array of data
   *)
  let queue_t = L.struct_type context [| i32_t; i32_t; data_double_ptr; pointer_t |] in
  let queue_ptr = L.pointer_type queue_t in
  let arg_t = L.struct_type context [| pointer_t; pointer_t; queue_ptr; queue_ptr |] in
  let arg_ptr = L.pointer_type arg_t in
  (* Utility to build index of gep *)
  let gep_index i = L.const_int i32_t i in

  (* Given a AST type, return a tag code *)
  let rec ocaml_tag = function
        A.Int -> [0]
      | A.Float -> [1]
      | A.String -> [2]
      | A.Bool -> [3]
      | A.Tuple (ty1, ty2) -> 4 :: ocaml_tag ty1 @ ocaml_tag ty2
      | A.Array (typ, length) -> [5; length] @ ocaml_tag typ
      | A.Thread -> [6]
      | A.Semaphore -> [7]
      | A.Void -> raise (Failure "Semantic bug: Void should not be tagged in pattern generation") in
  let rec tag_pattern = function
      | SBasePattern (typ, _) -> ocaml_tag typ
      | STuplePattern (pattern1, pattern2) ->
        let tag1 = tag_pattern pattern1 in
        let tag2 = tag_pattern pattern2 in
        4 :: (tag1 @ tag2)
      | SWildcardPattern -> [-1] in
  let tag_of_type = function
          A.Int -> (L.const_int i32_t 0)
        | A.Float -> (L.const_int i32_t 1)
        | A.String -> (L.const_int i32_t 2)
        | A.Bool -> (L.const_int i32_t 3)
        | A.Tuple _ -> (L.const_int i32_t 4)
        | A.Array _ -> (L.const_int i32_t 5)
        | A.Thread -> (L.const_int i32_t 6)
        | A.Semaphore -> (L.const_int i32_t 7)
        | _ -> raise (Failure "Semantic bug: Void should not be tagged") in
  let lltype_of_typ = function
          A.Int -> lltype_of_primitive A.Int
        | A.Bool  -> lltype_of_primitive A.Bool
        | A.Float -> lltype_of_primitive A.Float
        | A.Void  -> lltype_of_primitive A.Void
        | A.String -> lltype_of_primitive A.String
        | A.Thread -> queue_ptr
        | A.Array _ -> array_ptr
        | A.Tuple _ -> data_ptr
        | A.Semaphore -> sem_ptr
  and cast_llvalue_to_ptr typ llvalue builder = L.build_bitcast llvalue pointer_t (A.string_of_typ typ ^ "_to_ptr") builder
  and build_queue_gep queue builder =
    let size = L.build_in_bounds_gep queue [| gep_index 0; gep_index 0 |] "gep_size" builder and
        cap =  L.build_in_bounds_gep queue [| gep_index 0; gep_index 1 |] "gep_cap" builder and
        array = L.build_in_bounds_gep queue [| gep_index 0; gep_index 2 |] "gep_array" builder and
        mutex = L.build_in_bounds_gep queue [| gep_index 0; gep_index 3 |] "gep_mutex" builder
    in { size = size; cap = cap; array = array; mutex = mutex }
  and build_data_gep data builder =
    let tag = L.build_in_bounds_gep data [| gep_index 0; gep_index 0 |] "gep_tag" builder and
        head =  L.build_in_bounds_gep data [| gep_index 0; gep_index 1 |] "gep_head" builder and
        tail = L.build_in_bounds_gep data [| gep_index 0; gep_index 2 |] "gep_tail" builder
    in { tag = tag; head = head; tail = tail }
  and build_arg_gep data builder =
    let parent_queue = L.build_in_bounds_gep data [| gep_index 0; gep_index 2 |] "gep_parent_queue" builder and
        child_queue = L.build_in_bounds_gep data [| gep_index 0; gep_index 3 |] "gep_child_queue" builder
    in { parent_queue = parent_queue; child_queue = child_queue }
  and build_array_gep array_struct builder =
    let size = L.build_in_bounds_gep array_struct [| gep_index 0; gep_index 0 |] "gep_array_size" builder and
        array = L.build_in_bounds_gep array_struct [| gep_index 0; gep_index 1 |] "gep_array_array" builder and
        tags = L.build_in_bounds_gep array_struct [| gep_index 0; gep_index 2 |] "gep_array_tags" builder and
        tags_size = L.build_in_bounds_gep array_struct [| gep_index 0; gep_index 3 |] "gep_array_tags_size" builder in
    { size = size; data_array = array; tags = tags; tags_size = tags_size }
  and build_sem_gep sem builder =
    let count = L.build_in_bounds_gep sem [| gep_index 0; gep_index 0 |] "gep_count" builder and
        mutex = L.build_in_bounds_gep sem [| gep_index 0; gep_index 1 |] "gep_mutex" builder in
    { mutex = mutex; count = count}
  in

  let mutex_init_t : L.lltype = L.function_type void_t [| double_ptr |] in
  let mutex_init_func : L.llvalue = L.declare_function "mutex_init" mutex_init_t the_module in

  let mutex_lock_t : L.lltype = L.function_type void_t [| pointer_t |] in
  let mutex_lock_func : L.llvalue = L.declare_function "mutex_lock" mutex_lock_t the_module in

  let mutex_unlock_t : L.lltype = L.function_type void_t [| pointer_t |] in
  let mutex_unlock_func : L.llvalue = L.declare_function "mutex_unlock" mutex_unlock_t the_module in

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
   * C util functions
   *)
  let printf_t : L.lltype =
    L.var_arg_function_type (lltype_of_typ Int) [| L.pointer_type i8_t |] in
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

  let assert_t : L.lltype = L.function_type void_t [| (lltype_of_typ Int); pointer_t |] in
  let assert_func : L.llvalue = L.declare_function "assert_func" assert_t the_module in

  let exit_t : L.lltype = L.function_type void_t [| (lltype_of_typ Int) |] in
  let exit_func : L.llvalue = L.declare_function "exit" exit_t the_module in

  let float_floor_t = L.function_type float_t [| float_t |] in
  let float_floor_func : L.llvalue = L.declare_function "floor" float_floor_t the_module in

  let float_ceil_t = L.function_type float_t [| float_t |] in
  let float_ceil_func : L.llvalue = L.declare_function "ceil" float_ceil_t the_module in

  let float_round_t = L.function_type float_t [| float_t |] in
  let float_round_func : L.llvalue = L.declare_function "round" float_round_t the_module in

  let sqrt_t = L.function_type float_t [| float_t |] in
  let sqrt_func : L.llvalue = L.declare_function "sqrt" sqrt_t the_module in

  let scanf_t = L.var_arg_function_type i32_t [| pointer_t |] in
  let scanf_func : L.llvalue = L.declare_function "scanf" scanf_t the_module in

  let sscanf_t = L.var_arg_function_type i32_t [| pointer_t; pointer_t |] in
  let sscanf_func : L.llvalue = L.declare_function "sscanf" sscanf_t the_module in

  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder)

  (* Semaphore implementation *)
  (*
   * Initialize a semaphore and return a pointer to it
   *
   * @param i32_t The initial value of the semaphore
   * @return sem_ptr
   *)
  in let sem_init_func =
    let sem_init_t = L.function_type sem_ptr [| (lltype_of_typ Int) |] in
    let sem_init_func = L.define_function "Sem_init" sem_init_t the_module in

    let builder = L.builder_at_end context (L.entry_block sem_init_func) in

    let sem_malloc = L.build_malloc sem_t "sem_malloc" builder in
    let mutex_alloca = L.build_alloca pointer_t "mutex_alloca" builder in
    let count = L.param sem_init_func 0 in

    let { mutex = mutex_ptr; count = count_ptr } = build_sem_gep sem_malloc builder in
    let _ = L.build_call mutex_init_func [| mutex_alloca |] "" builder in
    let mutex = L.build_load mutex_alloca "mutex_load" builder in
    let _ = L.build_store mutex mutex_ptr builder in
    let _ = L.build_store count count_ptr builder in
    let _ = L.build_ret sem_malloc builder in
    sem_init_func

  (*
   * Acquire a lock or spin-wait
   *
   * @param sem_ptr
   * @return None
   *)
  in let sem_wait_func =
    let sem_wait_t = L.function_type void_t [| sem_ptr |] in
    let sem_wait_func = L.define_function "Sem_wait" sem_wait_t the_module in

    let wait_bb = L.append_block context "wait" sem_wait_func in
    let test_bb = L.append_block context "test_count" sem_wait_func in
    let unlock_bb = L.append_block context "unlock_mutex" sem_wait_func in
    let decrement_bb = L.append_block context "decrement_count" sem_wait_func in

    let builder = L.builder_at_end context (L.entry_block sem_wait_func) in
    let wait_builder = L.builder_at_end context wait_bb in
    let test_builder = L.builder_at_end context test_bb in
    let unlock_builder = L.builder_at_end context unlock_bb in
    let decrement_builder = L.builder_at_end context decrement_bb in

    (* Load mutex and jump to wait block *)
    let { mutex = mutex_ptr; count = count_ptr } = build_sem_gep (L.param sem_wait_func 0) builder in
    let mutex = L.build_load mutex_ptr "mutex_load" builder in
    let _ = L.build_br wait_bb builder in

    (* Lock the mutex and jump to test block *)
    let _ = L.build_call mutex_lock_func [| mutex |] "" wait_builder in
    let _ = L.build_br test_bb wait_builder in

    (* Test value count *)
    let count = L.build_load count_ptr "count_load" test_builder in
    let available = L.build_icmp L.Icmp.Sgt count (L.const_int (lltype_of_typ Int) 0) "test_count" test_builder in
    let _ = L.build_cond_br available decrement_bb unlock_bb test_builder in

    (* Unlock mutex and retry *)
    let _ = L.build_call mutex_unlock_func [| mutex |] "" unlock_builder in
    let _ = L.build_br wait_bb unlock_builder in

    (* Decrement count and unlock mutex *)
    let count = L.build_load count_ptr "count_load" decrement_builder in
    let count_decrement = L.build_sub count (L.const_int (lltype_of_typ Int) 1) "count_decrement" decrement_builder in
    let _ = L.build_store count_decrement count_ptr decrement_builder in
    let _ = L.build_call mutex_unlock_func [| mutex |] "" decrement_builder in
    let _ = add_terminal decrement_builder L.build_ret_void in
    sem_wait_func

  (*
   * Release the semaphore
   *)
  in let sem_post_func =
    let sem_post_t = L.function_type void_t [| sem_ptr |] in
    let sem_post_func = L.define_function "Sem_post" sem_post_t the_module in

    let builder = L.builder_at_end context (L.entry_block sem_post_func) in
    let { mutex = mutex_ptr; count = count_ptr } = build_sem_gep (L.param sem_post_func 0) builder in
    let mutex = L.build_load mutex_ptr "mutex_load" builder in
    let _ = L.build_call mutex_lock_func [| mutex |] "" builder in
    let count = L.build_load count_ptr "count_load" builder in
    let count_increment = L.build_add count (L.const_int (lltype_of_typ Int) 1) "count_increment" builder in
    let _ = L.build_store count_increment count_ptr builder in
    let _ = L.build_call mutex_unlock_func [| mutex |] "" builder in
    let _ = add_terminal builder L.build_ret_void in
    sem_post_func

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
        array_malloc = L.build_array_malloc data_ptr capacity "data_malloc" builder and
        mutex_alloca = L.build_alloca pointer_t "mutex_alloca" builder in

    let _ = L.build_store queue_malloc queue_alloca builder and
        _ = L.build_store array_malloc array_alloca builder in

    let queue = L.build_load queue_alloca "queue_load" builder and
        array = L.build_load array_alloca "array_load" builder and
        _ = L.build_call mutex_init_func [| mutex_alloca |] "" builder in

    let { size = size_ptr; cap = cap_ptr; array = array_ptr; mutex = mutex_ptr } = build_queue_gep queue builder in

    let mutex = L.build_load mutex_alloca "mutex_load" builder in
    let _ = L.build_store (L.const_int i32_t 0) size_ptr builder and
        _ = L.build_store capacity cap_ptr builder and
        _ = L.build_store array array_ptr builder and
        _ = L.build_store mutex mutex_ptr builder and
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
    let { size = size_ptr; mutex = mutex_ptr; _ } = build_queue_gep queue builder in
    let mutex = L.build_load mutex_ptr "mutex_load" builder in
    let _ = L.build_call mutex_lock_func [| mutex |] "" builder in
    let size = L.build_load size_ptr "size_load" builder in
    let empty = L.build_icmp L.Icmp.Eq size (L.const_int i32_t 0) "queue_empty" builder in
    let _ = L.build_call mutex_unlock_func [| mutex |] "" builder in
    let _ = add_terminal builder (L.build_ret empty) in queue_empty_func in

  (*
   * Push a data_ptr onto the queue
   *
   * @param queue_ptr
   * @param data_ptr
   * @return None
   *)
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
    let { mutex = mutex_ptr; size = size_ptr; cap = capacity_ptr; array = array_ptr } = build_queue_gep queue builder in
    let mutex = L.build_load mutex_ptr "mutex_load" builder in
    let _ = L.build_call mutex_lock_func [| mutex |] "" builder in
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

    let old_data_array = L.build_load array_ptr "old_data_array_load" copy_builder in
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

    let new_data_array = L.build_load data_array_alloca "new_data_array_load" terminate_builder in
    let _ = L.build_store new_data_array array_ptr terminate_builder in
    let new_capacity = L.build_mul capacity (L.const_int i32_t 2) "double_capacity" terminate_builder in
    let _ = L.build_store new_capacity capacity_ptr terminate_builder in
    let _ = L.build_br push_bb terminate_builder in

    let data = L.build_load data_alloca "data_load" push_builder in
    let data_array = L.build_load array_ptr "data_load" push_builder in
    let data_ptr = L.build_in_bounds_gep data_array [| size |] "gep_data_ptr" push_builder in
    let _ = L.build_store data data_ptr push_builder in
    let new_size = L.build_add size (L.const_int i32_t 1) "increment_size" push_builder in
    let _ = L.build_store new_size size_ptr push_builder in
    let _ = L.build_call mutex_unlock_func [| mutex |] "" push_builder in
    let _ = add_terminal push_builder L.build_ret_void in queue_push_func

  (*
   * Pop data from the queue. The function assume that the queue is not empty
   *
   * @param queue_ptr
   * @return None
   *)
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
    let { mutex = mutex_ptr; array = array_ptr; size = size_ptr; _ } = build_queue_gep queue builder in
    let mutex = L.build_load mutex_ptr "mutex_load" builder in
    let _ = L.build_call mutex_lock_func [| mutex |] "" builder in
    let data_array = L.build_load array_ptr "data_array_load" builder in
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
    let _ = L.build_call mutex_unlock_func [| mutex |] "" pop_builder in
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

  (*
   * Compare that the array tag from data_ptr matches the generated tag
   *
   * @param *i32_t Pointer to the tag array generated from the receive pattern
   * @param *i32_t Index in the tag array generated from the receive pattern
   * @param i32_t Length of the tag array generated from the receive pattern
   * @param *i32_t Pointer to the array identifier generated from incoming message
   *)
  let compare_array_tag_func =
    let compare_array_tag_t = L.function_type i1_t [| L.pointer_type i32_t; L.pointer_type i32_t; i32_t; array_ptr |] in
    let compare_array_tag_func = L.define_function "compare_array_tag" compare_array_tag_t the_module in

    let pred_bb = L.append_block context "pred_bb" compare_array_tag_func in
    let pred_builder = L.builder_at_end context pred_bb in

    let cmp_bb = L.append_block context "compare_bb" compare_array_tag_func in
    let cmp_builder = L.builder_at_end context cmp_bb in

    let increment_bb = L.append_block context "increment_bb" compare_array_tag_func in
    let increment_builder = L.builder_at_end context increment_bb in

    let ret_bb = L.append_block context "ret_bb" compare_array_tag_func in
    let ret_builder = L.builder_at_end context ret_bb in

    let builder = L.builder_at_end context (L.entry_block compare_array_tag_func) in

    let tag_ptr = L.param compare_array_tag_func 0 in
    let index_ptr = L.param compare_array_tag_func 1 in
    let tag_length = L.param compare_array_tag_func 2 in
    let array_struct_ptr = L.param compare_array_tag_func 3 in

    let array_index_alloca = L.build_alloca i32_t "index" builder in
    let _ = L.build_store (L.const_int i32_t 1) array_index_alloca builder in
    let { tags = array_tags_ptr; tags_size = array_tags_size_ptr; _ } = build_array_gep array_struct_ptr builder in
    let array_tags_size = L.build_load array_tags_size_ptr "array_tags_size_load" builder in
    let array_tags = L.build_load array_tags_ptr "array_tags_load" builder in
    let _ = L.build_br pred_bb builder in

    (* Check both arrays are in-bound *)
    let index = L.build_load index_ptr "index_load" pred_builder in
    let array_index = L.build_load array_index_alloca "array_index_load" pred_builder in
    let tag_inbound = L.build_icmp L.Icmp.Slt index tag_length "index_inbound" pred_builder in
    let array_inbound = L.build_icmp L.Icmp.Slt array_index array_tags_size "array_inbound" pred_builder in
    let continue = L.build_and tag_inbound array_inbound "both_inbound" pred_builder in
    let _ = L.build_cond_br continue cmp_bb ret_bb pred_builder in

    (* Compare the 2 tag arrays *)
    let tag_index_ptr = L.build_in_bounds_gep tag_ptr [| index |] "tag_index_gep" cmp_builder in
    let tag_value = L.build_load tag_index_ptr "tag_index_load" cmp_builder in
    let array_index_ptr = L.build_in_bounds_gep array_tags [| array_index |] "array_index_gep" cmp_builder in
    let array_value = L.build_load array_index_ptr "array_value_load" cmp_builder in
    let eq = L.build_icmp L.Icmp.Eq tag_value array_value "tag_equal" cmp_builder in
    let _ = L.build_cond_br eq increment_bb ret_bb cmp_builder in
    (* let _ = L.build_br increment_bb cmp_builder in *)

    (* Increment both indices *)
    let index_increment = L.build_add index (L.const_int i32_t 1) "index_increment" increment_builder in
    let _ = L.build_store index_increment index_ptr increment_builder in
    let array_index_increment = L.build_add array_index (L.const_int i32_t 1) "array_index_increment" increment_builder in
    let _ = L.build_store array_index_increment array_index_alloca increment_builder in
    let _ = L.build_br pred_bb increment_builder in

    let array_index = L.build_load array_index_alloca "array_index_load" ret_builder in
    let matched = L.build_icmp L.Icmp.Eq array_index array_tags_size "array_match" ret_builder in
    let _ = add_terminal ret_builder (L.build_ret matched) in compare_array_tag_func
  in

  (*
   * Receive cases each get a “tag” which corresponds to a (tuple-nested) Untangled type
     * Each primitive type has an integer code (see tag_of_type near the top of this file)
       * 0 -> int
       * 1 -> float
       * 2 -> string
       * 3 -> bool
       * 4 -> tuple - the following two “items” in the tag refer to the fst/snd of the tuple
       * 5 -> array - the following item in the tag refers to the element type of the array
       * 6 -> wildcard
     * For example, a tuple like (bool, float) would be [2, 3, 1]
                and an array like [(int, (string, bool))] would be [5, 4, 0, 4, 2, 3]
                and an array like [((int, string), bool)] would be [5, 4, 4, 0, 2, 3]
     * A tag encodes the structure of a tuple-nested Untangled type in depth-first order
   * This is a function that you can jump to using L.build_call, which takes four arguments:
     * 1. The tag to compare against
     * 2. An “index” specifying the index of the tag we’re examining
     * 3. The total length of the tag (an integer)
     * 4. The Untangled data structure that we’re inspecting against the tag
   * This function eventually returns true if the tag matches the Untangled data structure, and false otherwise.
  *)
  let tag_compare_func =
    (* Declare the function *)
    let tag_compare_t = L.function_type i1_t [| L.pointer_type i32_t; L.pointer_type i32_t; i32_t; data_ptr |] in
    let tag_compare_func = L.define_function "tag_compare" tag_compare_t the_module in

    (* Unpack the parameters we’re passed *)
    let tag_ptr = L.param tag_compare_func 0 in
    let index_ptr = L.param tag_compare_func 1 in
    let length = L.param tag_compare_func 2 in
    let data_t_ptr = L.param tag_compare_func 3 in
    let builder = L.builder_at_end context (L.entry_block tag_compare_func) in
    let index_loaded = L.build_load index_ptr "index_load" builder in

    (* let _ = L.build_call printf_func [| L.build_global_stringptr "  checking tag index %d\n" "fmt" builder; index_loaded |] "print_test" builder in *)
    let { tag = data_tag_ptr; head = head_ptr; tail = tail_ptr } = build_data_gep data_t_ptr builder in
    (* Get the integer tag of the first item in the “data” we’re examining *)
    let data_tag_val = L.build_load data_tag_ptr "data_tag_val" builder in
    (* let _ = L.build_call printf_func [| L.build_global_stringptr "    data has tag: %d\n" "fmt" builder; data_tag_val |] "print_test" builder in *)

    (* Returns false. We jump here whenever we find out that a tag definitely *doesn’t* match. *)
    let false_bb = L.append_block context "false_bb" tag_compare_func in
    let false_builder = L.builder_at_end context false_bb in
    (* let _ = L.build_call printf_func [| L.build_global_stringptr "    returning false\n" "fmt" builder; (L.const_int i32_t 0) |] "print_test" false_builder in *)
    let _ = L.build_ret (L.const_int i1_t 0) false_builder in

    (* Returns true. We jump here whenever we find out that a tag definitely *does* match. *)
    let true_bb = L.append_block context "true_bb" tag_compare_func in
    let true_builder = L.builder_at_end context true_bb in
    (* let _ = L.build_call printf_func [| L.build_global_stringptr "    returning true\n" "fmt" builder; (L.const_int i32_t 0) |] "print_test" true_builder in *)
    let _ = L.build_ret (L.const_int i1_t 1) true_builder in

    (* Other blocks we will fill and use below *)
    let wcard_bb = L.append_block context "wcard_bb" tag_compare_func in
    let wcard_builder = L.builder_at_end context wcard_bb in
    let equiv_bb = L.append_block context "equiv_bb" tag_compare_func in
    let equiv_builder = L.builder_at_end context equiv_bb in
    let recurse_bb = L.append_block context "recurse_bb" tag_compare_func in
    let recurse_builder = L.builder_at_end context recurse_bb in
    let check_arr_bb = L.append_block context "check_arr_bb" tag_compare_func in
    let check_arr_builder = L.builder_at_end context check_arr_bb in
    let arr_bb = L.append_block context "arr_bb" tag_compare_func in
    let arr_builder = L.builder_at_end context arr_bb in
    let base_bb = L.append_block context "base_bb" tag_compare_func in
    let base_builder = L.builder_at_end context base_bb in
    let check_base_bb = L.append_block context "check_base_bb" tag_compare_func in
    let check_base_builder = L.builder_at_end context check_base_bb in

    (* 1. Check that the index is in bounds. If it’s not, return false. *)
    let oob = L.build_icmp L.Icmp.Eq index_loaded length "index_comparison" builder in
    (* let _ = L.build_call printf_func [| L.build_global_stringptr "    oob? %d\n" "fmt" builder; oob |] "print_test" builder in *)
    let _ = L.build_cond_br oob false_bb wcard_bb builder in
    (*
     * Once we know the index is in bounds, it’s safe to unpack the value from the tag.
     * We won’t load the tag again, so this is a safe place to increment the index (we know that the
     * next recursive step, if any, will want to examine the next value of the tag). *)
    let array_tag_ptr = L.build_in_bounds_gep tag_ptr [| index_loaded |] "gep_array_tag" wcard_builder in
    let tag_val = L.build_load array_tag_ptr "array_tag_load" wcard_builder in
    (* let _ = L.build_call printf_func [| L.build_global_stringptr "    pattern wants tag: %d\n" "fmt" wcard_builder; tag_val |] "print_test" wcard_builder in *)
    let incremented_index = L.build_add (L.const_int i32_t 1) index_loaded "increment_index" wcard_builder in
    let _ = L.build_store incremented_index index_ptr wcard_builder in

    (* 2. Check if the tag contains a wildcard at this position. If it does, return true. *)
    let wcard_pred = L.build_icmp L.Icmp.Eq tag_val (L.const_int i32_t (-1)) "wildcard_pred" wcard_builder in
    let _ = L.build_cond_br wcard_pred true_bb equiv_bb wcard_builder in

    (* 3. Make sure this next value in the tag array—if it’s not a wildcard—matches the tag on the data. If it doesn’t, return false. *)
    let equiv_pred = L.build_icmp L.Icmp.Eq tag_val data_tag_val "equiv_pred" equiv_builder in
    let _ = L.build_cond_br equiv_pred check_base_bb false_bb equiv_builder in

    (* 4. If the type is a tuple, recurse to check both the head and the tail *)
    let is_tuple = L.build_icmp L.Icmp.Eq data_tag_val (L.const_int i32_t 4) "pred" check_base_builder in
    let _ = L.build_cond_br is_tuple recurse_bb check_arr_bb check_base_builder in

    (* 5. If the type is an array, compare the tag array *)
    let is_array = L.build_icmp L.Icmp.Eq data_tag_val (L.const_int i32_t 5) "pred" check_arr_builder in
    let _ = L.build_cond_br is_array arr_bb true_bb check_arr_builder in

    (* 6. Type has been determined as array, check tail to verify length & base type *)
    let head_load = L.build_load head_ptr "head_load" arr_builder in
    let head_cast = L.build_bitcast head_load array_ptr "tail_cast" arr_builder in
    let pred = L.build_call compare_array_tag_func [| tag_ptr; index_ptr; length; head_cast |] "array_tag_pred" arr_builder in
    let _ = L.build_cond_br pred true_bb false_bb arr_builder in

    (* 7. If the type is a tuple type, compare the tag array *)
    let head_load = L.build_load head_ptr "head_load" recurse_builder in
    let head_cast = L.build_bitcast head_load data_ptr "head_cast" recurse_builder in
    let tail_load =  L.build_load tail_ptr "tail_load" recurse_builder in
    let tail_cast = L.build_bitcast tail_load data_ptr "tail_cast" recurse_builder in
    let head_matches = L.build_call tag_compare_func [| tag_ptr; index_ptr; length; head_cast |] "head_compare" recurse_builder in
    let tail_matches = L.build_call tag_compare_func [| tag_ptr; index_ptr; length; tail_cast |] "tail_compare" recurse_builder in
    let both_match = L.build_and head_matches tail_matches "anded_compare" recurse_builder in
    let _ = L.build_ret both_match recurse_builder in

    (* 8. If all those checked passed, return true. *)
    let _ = L.build_br true_bb base_builder in

    tag_compare_func
  in

  (*
   * Raise float base to exponent
   *
   * @param float_t Base value
   * @param Int Exponent
   * @param float_t Result
   *)
  let float_pow_func =
    let float_pow_t = L.function_type float_t [| float_t; (lltype_of_typ Int) |] in
    let float_pow_func = L.define_function "float_pow" float_pow_t the_module in

    let check_bb = L.append_block context "operand_test" float_pow_func in
    let zero_bb = L.append_block context "zero_test" float_pow_func in
    let neg_bb = L.append_block context "neg_test" float_pow_func in
    let neg_1_bb = L.append_block context "neg_1_test" float_pow_func in
    let set_0_bb = L.append_block context "set_0" float_pow_func in
    let pow_bb = L.append_block context "power" float_pow_func in
    let ret_bb = L.append_block context "return" float_pow_func in

    let builder = L.builder_at_end context (L.entry_block float_pow_func) in
    let check_builder = L.builder_at_end context check_bb in
    let zero_builder = L.builder_at_end context zero_bb in
    let neg_builder = L.builder_at_end context neg_bb in
    let neg_1_builder = L.builder_at_end context neg_1_bb in
    let set_0_builder = L.builder_at_end context set_0_bb in
    let pow_builder = L.builder_at_end context pow_bb in
    let ret_builder = L.builder_at_end context ret_bb in

    (* Allocation *)
    let base_alloca = L.build_alloca float_t "base_alloca" builder in
    let expo_alloca = L.build_alloca (lltype_of_typ Int) "expo_alloca" builder in
    let res_alloca = L.build_alloca float_t "res_alloca" builder in

    let _ = L.build_store (L.param float_pow_func 0) base_alloca builder in
    let _ = L.build_store (L.param float_pow_func 1) expo_alloca builder in
    (* Result is 1 if exponent = 0 *)
    let _ = L.build_store (L.const_float float_t 1.0) res_alloca builder in
    let _ = L.build_br check_bb builder in

    (* Check that base is not 0 and exponent is non-negative *)
    let msg = L.build_global_stringptr "0.0 can't be raised to negative power" "pow_msg" check_builder in
    let base = L.build_load base_alloca "base_load" check_builder in
    let expo = L.build_load expo_alloca "expo_load" check_builder in
    let zero = L.build_fcmp L.Fcmp.Oeq base (L.const_float float_t 0.0) "base_is_zero" check_builder in
    let negative = L.build_icmp L.Icmp.Slt expo (L.const_int (lltype_of_typ Int) 0) "expo_is_negative" check_builder in
    let error = L.build_and zero negative "pow_error" check_builder in
    let valid = L.build_not error "negate_error" check_builder in
    let valid_cast = L.build_intcast valid (lltype_of_typ Int) "valid_cast" check_builder in
    let _ = L.build_call assert_func [| valid_cast; msg |] "" check_builder in
    let _ = L.build_br zero_bb check_builder in

    (* Test if exponent = 0 *)
    let expo = L.build_load expo_alloca "expo_load" zero_builder in
    let zero = L.build_icmp L.Icmp.Eq expo (L.const_int (lltype_of_typ Int) 0) "expo_is_zero" zero_builder in
    let _ = L.build_cond_br zero ret_bb neg_bb zero_builder in

    (* Test if exponent < 0 *)
    let expo = L.build_load expo_alloca "expo_load" neg_builder in
    let negative = L.build_icmp L.Icmp.Slt expo (L.const_int (lltype_of_typ Int) 0) "expo_is_negative" neg_builder in
    let _ = L.build_cond_br negative neg_1_bb pow_bb neg_builder in

    (* Test if exponent = -1 and base = 1 *)
    let base = L.build_load base_alloca "base_load" neg_1_builder in
    let expo = L.build_load expo_alloca "expo_load" neg_1_builder in
    let negative_1 = L.build_icmp L.Icmp.Eq expo (L.const_int (lltype_of_typ Int) (-1)) "expo_is_-1" neg_1_builder in
    let base_1 = L.build_fcmp L.Fcmp.Oeq base (L.const_float float_t 1.0) "base_is_1" neg_1_builder in
    let both_1 = L.build_and base_1 negative_1 "both_1" neg_1_builder in
    (* If it's -1, return because res_alloca is currently 1 *)
    let _ = L.build_cond_br both_1 ret_bb set_0_bb neg_1_builder in

    (* Set result to 0 if exponent < - 1 *)
    let _ = L.build_store (L.const_float float_t 0.0) res_alloca set_0_builder in
    let _ = L.build_br ret_bb set_0_builder in

    (* Take power and decrement exponent *)
    let base = L.build_load base_alloca "base_load" pow_builder in
    let expo = L.build_load expo_alloca "expo_load" pow_builder in
    let res = L.build_load res_alloca "res_load" pow_builder in
    let pow = L.build_fmul base res "raise_res" pow_builder in
    let expo_decrement = L.build_sub expo (L.const_int (lltype_of_typ Int) 1) "expo_sub" pow_builder in
    let _ = L.build_store pow res_alloca pow_builder in
    let _ = L.build_store expo_decrement expo_alloca pow_builder in
    let _ = L.build_br zero_bb pow_builder in

    let res = L.build_load res_alloca "res_load" ret_builder in
    let _ = add_terminal ret_builder (L.build_ret res) in float_pow_func

  (*
   * Raise int base to exponent
   *
   * @param Int Base value
   * @param Int Exponent
   * @param Int Result
   *)
  in let int_pow_func =
    let int_pow_t = L.function_type (lltype_of_typ Int) [| (lltype_of_typ Int); (lltype_of_typ Int) |] in
    let int_pow_func = L.define_function "int_pow" int_pow_t the_module in

    let check_bb = L.append_block context "operand_test" int_pow_func in
    let zero_bb = L.append_block context "zero_test" int_pow_func in
    let neg_bb = L.append_block context "neg_test" int_pow_func in
    let neg_1_bb = L.append_block context "neg_1_test" int_pow_func in
    let set_0_bb = L.append_block context "set_0" int_pow_func in
    let pow_bb = L.append_block context "power" int_pow_func in
    let ret_bb = L.append_block context "return" int_pow_func in

    let builder = L.builder_at_end context (L.entry_block int_pow_func) in
    let check_builder = L.builder_at_end context check_bb in
    let zero_builder = L.builder_at_end context zero_bb in
    let neg_builder = L.builder_at_end context neg_bb in
    let neg_1_builder = L.builder_at_end context neg_1_bb in
    let set_0_builder = L.builder_at_end context set_0_bb in
    let pow_builder = L.builder_at_end context pow_bb in
    let ret_builder = L.builder_at_end context ret_bb in

    (* Allocation *)
    let base_alloca = L.build_alloca (lltype_of_typ Int) "base_alloca" builder in
    let expo_alloca = L.build_alloca (lltype_of_typ Int) "expo_alloca" builder in
    let res_alloca = L.build_alloca (lltype_of_typ Int) "res_alloca" builder in

    let _ = L.build_store (L.param int_pow_func 0) base_alloca builder in
    let _ = L.build_store (L.param int_pow_func 1) expo_alloca builder in
    let _ = L.build_store (L.const_int (lltype_of_typ Int) 1) res_alloca builder in
    let _ = L.build_br check_bb builder in

    (* Check that base is not 0 and exponent is non-negative *)
    let msg = L.build_global_stringptr "0 can't be raised to negative power" "pow_msg" check_builder in
    let base = L.build_load base_alloca "base_load" check_builder in
    let expo = L.build_load expo_alloca "expo_load" check_builder in
    let zero = L.build_icmp L.Icmp.Eq base (L.const_int (lltype_of_typ Int) 0) "base_is_zero" check_builder in
    let negative = L.build_icmp L.Icmp.Slt expo (L.const_int (lltype_of_typ Int) 0) "expo_is_negative" check_builder in
    let error = L.build_and zero negative "pow_error" check_builder in
    let valid = L.build_not error "negate_error" check_builder in
    let valid_cast = L.build_intcast valid (lltype_of_typ Int) "valid_cast" check_builder in
    let _ = L.build_call assert_func [| valid_cast; msg |] "" check_builder in
    let _ = L.build_br zero_bb check_builder in

    (* Test if exponent = 0 *)
    let expo = L.build_load expo_alloca "expo_load" zero_builder in
    let zero = L.build_icmp L.Icmp.Eq expo (L.const_int (lltype_of_typ Int) 0) "expo_is_zero" zero_builder in
    let _ = L.build_cond_br zero ret_bb neg_bb zero_builder in

    (* Test if exponent < 0 *)
    let expo = L.build_load expo_alloca "expo_load" neg_builder in
    let negative = L.build_icmp L.Icmp.Slt expo (L.const_int (lltype_of_typ Int) 0) "expo_is_negative" neg_builder in
    let _ = L.build_cond_br negative neg_1_bb pow_bb neg_builder in

    (* Test if exponent = -1 and base = 1 *)
    let base = L.build_load base_alloca "base_load" neg_1_builder in
    let expo = L.build_load expo_alloca "expo_load" neg_1_builder in
    let negative_1 = L.build_icmp L.Icmp.Eq expo (L.const_int (lltype_of_typ Int) (-1)) "expo_is_-1" neg_1_builder in
    let base_1 = L.build_icmp L.Icmp.Eq base (L.const_int (lltype_of_typ Int) 1) "base_is_1" neg_1_builder in
    let both_1 = L.build_and negative_1 base_1 "both_1" neg_1_builder in
    (* If it's -1, return because res_alloca is currently 1 *)
    let _ = L.build_cond_br both_1 ret_bb set_0_bb neg_1_builder in

    (* Set result to 0 if exponent < - 1 *)
    let _ = L.build_store (L.const_int (lltype_of_typ Int) 0) res_alloca set_0_builder in
    let _ = L.build_br ret_bb set_0_builder in

    (* Take power and decrement exponent *)
    let base = L.build_load base_alloca "base_load" pow_builder in
    let expo = L.build_load expo_alloca "expo_load" pow_builder in
    let res = L.build_load res_alloca "res_load" pow_builder in
    let pow = L.build_mul base res "raise_res" pow_builder in
    let expo_decrement = L.build_sub expo (L.const_int (lltype_of_typ Int) 1) "expo_sub" pow_builder in
    let _ = L.build_store pow res_alloca pow_builder in
    let _ = L.build_store expo_decrement expo_alloca pow_builder in
    let _ = L.build_br zero_bb pow_builder in

    let res = L.build_load res_alloca "res_load" ret_builder in
    let _ = add_terminal ret_builder (L.build_ret res) in int_pow_func

  in let bool_of_string_func =
    let bool_of_string_t = L.function_type i1_t [| pointer_t |] in
    let bool_of_string_func = L.define_function "bool_of_string" bool_of_string_t the_module in

    let true_bb = L.append_block context "true_bb" bool_of_string_func in
    let false_bb = L.append_block context "false_bb" bool_of_string_func in

    let builder = L.builder_at_end context (L.entry_block bool_of_string_func) in
    let true_builder = L.builder_at_end context true_bb in
    let false_builder = L.builder_at_end context false_bb in

    let llvalue = L.param bool_of_string_func 0 in
    let err_msg = L.build_global_stringptr "bool_of_string expects true or false" "err_ msg" builder and
        true_value = L.build_global_stringptr "true" "#t" builder and
        false_value = L.build_global_stringptr "false" "#f" builder in
    let true_pred =  L.build_call string_equiv_func [| true_value; llvalue |] "true_cmp" builder in
    let false_pred = L.build_call string_equiv_func [| false_value; llvalue |] "false_cmp" builder in
    let valid = L.build_or true_pred false_pred "true_or_false" builder in
    let valid_cast = L.build_intcast valid i64_t "valid_cast" builder in
    let _ = L.build_call assert_func [| valid_cast; err_msg |] "" builder in
    let _ = L.build_cond_br true_pred true_bb false_bb builder in

    let _ = add_terminal true_builder (L.build_ret (L.const_int i1_t 1)) in
    let _ = add_terminal false_builder (L.build_ret (L.const_int i1_t 0)) in bool_of_string_func


  (* Wrapper around binop function for compatibility *)
  in let bin_op_wrapper fn =
    let wrapper e1 e2 name builder = L.build_call fn [| e1; e2 |] name builder in wrapper

  in let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
	      Array.of_list (List.map (fun (t,_) -> lltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (lltype_of_typ fdecl.sret_type) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty fdecls in

  let thread_decls =
    let thread_decl m tdecl =
      let stname = tdecl.stname and
          ttype = L.function_type (L.pointer_type i8_t) [| L.pointer_type i8_t |] in
      (* Add paramaters for threads *)
      StringMap.add stname (L.define_function tdecl.stname ttype the_module, tdecl) m in
    List.fold_left thread_decl StringMap.empty tdecls in

  let global_counter = L.define_global "global_counter" (L.const_int i64_t 1) the_module in
  let global_mutex = L.define_global "global_mutex"  (L.const_null pointer_t) the_module in

  let build_body ?(arg_gep : arg_gep option) (builder, env) sstmt the_thread =
    let string_format_str = L.build_global_stringptr "%s" "fmt" builder in
    let int_format_str = L.build_global_stringptr "%ld" "fmt" builder in
    let float_format_str = L.build_global_stringptr "%lf" "fmt" builder in
    let index_oob_str = L.build_global_stringptr "Index out of bounds" "fmt" builder in

    let rec expr ((builder: L.llbuilder), env) ((typ, sexpr : sexpr)) =
      match sexpr with
        | SIntLit i -> (L.const_int (lltype_of_typ Int) i, env)
        | SStringLit s -> (L.build_global_stringptr s "tmp" builder, env)
        | SBoolLit b -> (L.const_int i1_t (if b then 1 else 0), env)
        | SFloatLit l -> (L.const_float_of_string float_t l, env)
        | (STupleLit _) as sexpr  ->
            let rec build_tuple ptr builder (sexpr : sexpr) =
              let { tag = tag_ptr; head = head_ptr; tail = tail_ptr } = build_data_gep ptr builder in
                match sexpr with
                  (Array _ as ty, _) ->
                    let (llvalue, _) = expr (builder, env) sexpr in
                    let array_cast = L.build_pointercast llvalue pointer_t "array_cast" builder in
                    let _ = L.build_store (tag_of_type ty) tag_ptr builder in
                    let _ = L.build_store array_cast head_ptr builder in
                    let _ = L.build_store (L.const_null pointer_t) tail_ptr builder in
                    ()
                  | (ty, STupleLit (fst, snd)) ->
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
        | SArrayLit sexprs ->
            let size = L.const_int i32_t (List.length sexprs) in
            let base_lltype =
              match sexprs with
                | ((ty, _)::_) -> lltype_of_typ ty
                | _ -> raise (Failure "Cannot have empty array literals")
            in
            let (llvalues, env') = List.fold_left
              (fun (llvalues, env) sexpr ->
                let (llvalue, env') = (expr (builder, env) sexpr) in
              (llvalue::llvalues, env')) ([], env) sexprs
            in

            let array_struct_malloc = L.build_malloc array_t "array_struct_malloc" builder in
            let array_malloc = L.build_array_malloc base_lltype size "array_lit_malloc" builder in

            let _ = List.iteri
              (fun i llvalue ->
                let gep = L.build_gep array_malloc [| L.const_int i32_t i |] ("array_lit_gep " ^ string_of_int i) builder in
                let _ = L.build_store llvalue gep builder in ()) (List.rev llvalues)
            in

            let { size = size_ptr; data_array = array_ptr; tags = tags_ptr; tags_size = tags_size_ptr }
              = build_array_gep array_struct_malloc builder in

            let rec build_arr_identifier (typ : A.typ) = (match typ with
              | Tuple (t1, t2) -> 4 :: (build_arr_identifier t1) @ (build_arr_identifier t2)
              | Array (base_t, length) -> [5; length] @ (build_arr_identifier base_t)
              | _ -> ocaml_tag typ) in

            let base_typ = match sexprs with
                ((ty, _) :: _) -> ty
                | _ -> raise (Failure "Semantic bug: Can't have empty array literals") in

            let ocaml_tag = [5; List.length sexprs] @ (build_arr_identifier base_typ) in
            let length =  L.const_int i32_t (List.length ocaml_tag) in
            let tag_malloc = L.build_array_malloc i32_t length "tag_malloc" builder in
            let _ = List.mapi
              (fun i tag ->
                let index_ptr = L.build_in_bounds_gep tag_malloc [| L.const_int i32_t i |] "index_ptr" builder in
                ignore (L.build_store (L.const_int i32_t tag) index_ptr builder)
              ) ocaml_tag in

            (* let tag_cast = L.build_bitcast tag_malloc pointer_t "tag_cast" builder in *)
            let array_cast = L.build_bitcast array_malloc pointer_t "array_malloc" builder in
            let _ = L.build_store array_cast array_ptr builder in
            let _ = L.build_store tag_malloc tags_ptr builder in
            let _ = L.build_store size size_ptr builder in
            let _ = L.build_store length tags_size_ptr builder in (array_struct_malloc, env')
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
            let buf_malloc = L.build_array_malloc i8_t (L.const_int i32_t 20) "buf_malloc" builder in
            let buf_alloca = L.build_alloca (L.pointer_type i8_t) "buf_alloca" builder in
            let _ = L.build_store buf_malloc buf_alloca builder in
            let buf = L.build_load buf_alloca "buf_load" builder in
            let _ = (L.build_call sprintf_func [| buf ; int_format_str; llvalue |] "string_of_int" builder)
            in (buf, env')
        | SCall ("string_of_float", [sexpr]) ->
            let (llvalue, env') = expr (builder, env) sexpr in
            let buf_malloc = L.build_array_malloc i8_t (L.const_int i32_t 40) "buf_malloc" builder in
            let buf_alloca = L.build_alloca (L.pointer_type i8_t) "buf_alloca" builder in
            let _ = L.build_store buf_malloc buf_alloca builder in
            let buf = L.build_load buf_alloca "buf_load" builder in
            let _ = (L.build_call sprintf_func [| buf ; float_format_str; llvalue |]
                                                "string_of_float" builder)
            in (buf, env')
        | SCall ("int_of_string", [sexpr]) ->
            let (llvalue, env') = expr (builder, env) sexpr in
            let int_alloca = L.build_alloca i64_t "int_alloca" builder in
            let _ = L.build_call sscanf_func [| llvalue; int_format_str; int_alloca |] "num_matched" builder in
            let int_loaded = L.build_load int_alloca "int_loaded" builder in
            let buf_malloc = L.build_array_malloc i8_t (L.const_int i32_t 40) "buf_malloc" builder in
            let buf_alloca = L.build_alloca (L.pointer_type i8_t) "buf_alloca" builder in
            let _ = L.build_store buf_malloc buf_alloca builder in
            let buf = L.build_load buf_alloca "buf_load" builder in
            let _ = L.build_call sprintf_func [| buf; int_format_str; int_loaded |] "string_of_int" builder in
            let err_msg = L.build_global_stringptr "int_of_string could not parse int string" "err_msg" builder in
            let pred = L.build_call string_equiv_func [| buf; llvalue |] "string_eq" builder in
            let pred_cast = L.build_intcast pred i64_t "pred_cast" builder in
            let _ = L.build_call assert_func [| pred_cast; err_msg |] "" builder in
            let int_loaded = L.build_load int_alloca "int_loaded" builder in
            (int_loaded, env')
        | SCall ("float_of_string", [sexpr]) ->
            let (llvalue, env') = expr (builder, env) sexpr in
            let float_alloca = L.build_alloca float_t "float_alloca" builder in
            let num_matched = L.build_call sscanf_func [| llvalue; float_format_str; float_alloca |] "num_matched" builder in
            let err_msg = L.build_global_stringptr "float_of_string could not parse float string" "err_msg" builder in
            let pred = L.build_icmp L.Icmp.Eq num_matched (L.const_int i32_t 1) "pred" builder in
            let pred_cast = L.build_intcast pred i64_t "pred_cast" builder in
            let _ = L.build_call assert_func [| pred_cast; err_msg |] "" builder in
            let float_loaded = L.build_load float_alloca "float_loaded" builder in (float_loaded, env')
        | SCall ("bool_of_string", [sexpr]) ->
            let (llvalue, env') = expr (builder, env) sexpr in
            (L.build_call bool_of_string_func [| llvalue |] "string_of_bool" builder, env')
        | SCall ("floor", [sexpr]) ->
            let (llvalue, env') = expr (builder, env) sexpr in
            let float_floor = L.build_call float_floor_func [| llvalue |] "int_floor" builder in
            L.build_fptosi float_floor (lltype_of_typ Int) "int_floor_cast" builder, env'
        | SCall ("ceil", [sexpr]) ->
            let (llvalue, env') = expr (builder, env) sexpr in
            let float_ceil = L.build_call float_ceil_func [| llvalue |] "int_ceil" builder in
            L.build_fptosi float_ceil (lltype_of_typ Int) "int_ceil_cast" builder, env'
        | SCall ("round", [sexpr]) ->
            let (llvalue, env') = expr (builder, env) sexpr in
            let float_round = L.build_call float_round_func [| llvalue |] "int_round" builder in
            L.build_fptosi float_round (lltype_of_typ Int) "int_round_cast" builder, env'
        | SCall ("int_of_float", [sexpr]) -> expr (builder, env) (typ, SCall ("floor", [sexpr])) (* int_of_float is an alias of floor *)
        | SCall("float_of_int", [sexpr]) ->
            let (llvalue, env') = expr (builder, env) sexpr in
            L.build_sitofp llvalue float_t "float_of_int" builder, env'
        | SCall("sqrt", [sexpr]) ->
            let (llvalue, env') = expr (builder, env) sexpr in
            L.build_call sqrt_func [| llvalue |] "sqrt" builder, env'
        | SCall ("make_semaphore", [sexpr]) ->
            let (llvalue, env') = expr (builder, env) sexpr in
            let sem = L.build_call sem_init_func [| llvalue |] "sem_init" builder in
            (sem, env')
        | SCall ("exit", [sexpr]) -> (* “exit” exits the program *)
            let (llvalue, env') = expr (builder, env) sexpr in
            (L.build_call exit_func [| llvalue |] "" builder, env')
        | SCall ("end", []) -> (* “end” exits the thread *)
            let global_mutex_loaded = L.build_load global_mutex "global_mutex_load" builder in
            let _ = L.build_call mutex_lock_func [| global_mutex_loaded |] "" builder in
            let global_counter_load = L.build_load global_counter "global_counter_load" builder in
            let global_counter_load = L.build_sub global_counter_load (L.const_int i64_t 1) "sub_global_counter" builder in
            let _ = L.build_store global_counter_load global_counter builder in
            let _ = L.build_call mutex_unlock_func [| global_mutex_loaded |] "" builder in
            (L.build_ret (L.const_null pointer_t) builder, env)
        | SCall ("input", []) ->
            let buffer = L.build_array_malloc i8_t (L.const_int i32_t 1024) "buffer" builder in
            let _ = L.build_call scanf_func [| string_format_str; buffer |] "input" builder in
            (buffer, env)
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
                    | A.Pow              -> bin_op_wrapper float_pow_func
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
                    | A.Pow       -> bin_op_wrapper int_pow_func)
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
                | A.Bool ->
                  (match op with
                    | A.Equality -> L.build_icmp L.Icmp.Eq
                    | A.Neq -> L.build_icmp L.Icmp.Ne
                    | A.And -> L.build_and
                    | A.Or -> L.build_or
                    | _ -> raise (Failure "Boolean binary operation not supported")
                    )
                | A.Thread | A.Semaphore | A.Tuple _ | A.Array _ ->
                  (fun e1 e2 name builder ->
                    let lpointer = L.build_ptrtoint e1 i64_t (name ^ "_left_cast") builder in
                    let rpointer = L.build_ptrtoint e2 i64_t (name ^ "_left_cast") builder in
                    let equal = L.build_icmp L.Icmp.Eq lpointer rpointer "address_cmp" builder in
                    (match op with
                      A.Equality -> equal
                      | A.Neq -> L.build_not equal "negate_address" builder
                      | _ -> raise (Failure ("Semantic bug: Should have rejected " ^ A.string_of_op op))))
                | typ -> raise (Failure ("Semantic bug: " ^ A.string_of_typ typ ^ " does not support binop")))
            in (op e1' e2' "binop_result" builder, env'')
        | SIndex (id, sexpr) ->
            let (array_struct, _) = expr (builder, env) id in
            let { size = size_ptr; data_array = array_ptr; _ } = build_array_gep array_struct builder in

            let (ll_index, _) = expr (builder, env) sexpr in
            let ll_index = L.build_intcast ll_index i32_t "index_cast" builder in
            let size = L.build_load size_ptr "size_load" builder in
            let pred = L.build_icmp L.Icmp.Slt ll_index size "index_pred" builder in
            let pred = L.build_and pred (L.build_icmp L.Icmp.Sge ll_index (L.const_int i32_t 0) "index_pred" builder) "index_pred" builder in
            let pred = L.build_intcast pred i64_t "index_pred" builder in
            let _ = L.build_call assert_func [| pred; index_oob_str |] "" builder in
            let array = L.build_load array_ptr "array_load" builder in
            (* let _ = print_endline (A.string_of_typ typ) in *)
            let array = L.build_bitcast array (L.pointer_type (lltype_of_typ typ)) "array_cast" builder in
            let ptr = L.build_in_bounds_gep array [| ll_index |] "array_gep" builder in
            let res_value = L.build_load ptr "array_load" builder in
            (res_value, env)
        | SSpawn tn ->
          (* Increment thread count *)
          let global_mutex_loaded = L.build_load global_mutex "global_mutex_load" builder in
          let _ = L.build_call mutex_lock_func [| global_mutex_loaded |] "" builder in
          let thread_count = L.build_load global_counter "global_counter_load" builder in
          let thread_count = L.build_add thread_count (L.const_int i64_t 1) "thread_count" builder in
          let _ = L.build_store thread_count global_counter builder in
          let _ = L.build_call mutex_unlock_func [| global_mutex_loaded |] "" builder in
          (*
           * Generate child queue
           * Store parent, child into routine_t
           * Call pthread_create
           * Expression return child queue
           *)
          let { child_queue = self_queue_ptr; _ } = (match arg_gep with
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
          (child_queue, env);
        | SAssign (var_name, v) ->
            let (value_to_assign, env') = expr (builder, env) v in
            let storage = StringMap.find var_name env' in
            let _ = L.build_store value_to_assign storage builder in
            (value_to_assign, env')
        | SAssignIndex (array_sx, sindex, sexpr) ->
            let (array_loaded, _) = expr (builder, env) array_sx in
            let { size = size_ptr; data_array = data_ptr; _ } = build_array_gep array_loaded builder in
            let (ll_index, env') = expr (builder, env) sindex in
            let ll_index = L.build_intcast ll_index i32_t "index_cast" builder in

            let size = L.build_load size_ptr "size_load" builder in
            let pred = L.build_icmp L.Icmp.Slt ll_index size "index_pred" builder in
            let pred = L.build_and pred (L.build_icmp L.Icmp.Sge ll_index (L.const_int i32_t 0) "index_pred" builder) "index_pred" builder in
            let pred = L.build_intcast pred i64_t "index_pred" builder in
            let _ = L.build_call assert_func [| pred; index_oob_str |] "" builder in

            let (value_to_assign, env'') = expr (builder, env') sexpr in
            let array = L.build_load data_ptr "array_load" builder in
            let array = L.build_bitcast array (L.pointer_type (lltype_of_typ typ)) "array_cast" builder in
            let ptr = L.build_in_bounds_gep array [| ll_index |] "array_gep" builder in
            let _ = L.build_store value_to_assign ptr builder in
            (value_to_assign, env'')
        | SPreUnop (_, sexpr) ->
            let (llvalue, env') = expr (builder, env) sexpr in
            let negated = L.build_xor llvalue (L.const_int i1_t 1) "negate_bool" builder in
            (negated, env')
        | SPostUnop (op, sexpr) ->
            let (sem, env') = expr (builder, env) sexpr in
            let fn = match op with
                  Plusplus -> sem_post_func
                | Minmin -> sem_wait_func
                | _ -> raise (Failure ("Parsing bug: " ^ string_of_unop op sexpr ^ " should not be postfix")) in
            (L.build_call fn [| sem |] "" builder, env')
    and stmt ((builder: L.llbuilder), env, (ctx : stmt_context)) sstmt =
      match sstmt with
          SBlock sblock ->
            let (builder_final, _) = List.fold_left
              (fun (builder, env) sstmt -> stmt (builder, env, ctx) sstmt)
              (builder, env)
              sblock
            in
            (builder_final, env)
        | SExpr sexpr -> let (_, env2) = expr (builder, env) sexpr in (builder, env2)
        | SReturn sexpr ->
            let (typ, _) = sexpr in
            let _ = match typ with
              A.Void -> L.build_ret_void builder
              | _ ->
                let (value, _) = expr (builder, env) sexpr in L.build_ret value builder
            in (builder, env)
        | SDecl (SBaseDecl (ty, var_name, sx)) ->
            let (llvalue, env2) = expr (builder, env) sx in
            let alloca = L.build_alloca (lltype_of_typ ty) var_name builder in
            let llvalue =
              let default_value_of builder = function
                | A.Int -> L.const_int i64_t 0
                | A.Float -> L.const_float float_t 0.0
                | A.Semaphore | A.Thread -> L.const_null pointer_t
                | A.Bool -> L.const_int i1_t 0
                | A.String -> L.build_global_stringptr "" "tmp" builder
                | A.Void -> raise (Failure "Semantic bug: Can't have void in tuple type")
                | _ -> raise (Failure "Codegen bug: Should not be here") in

              let rec default_tuple ptr typ builder =
                let { tag = tag_ptr; head = head_ptr; tail = tail_ptr }
                  = build_data_gep ptr builder in

                let tag = tag_of_type typ in
                let _ = L.build_store tag tag_ptr builder in
                match typ with
                  | A.Array _ ->
                      let array_struct_malloc = L.build_malloc array_t "array_struct_malloc" builder in
                      let _ = default_array array_struct_malloc typ builder in
                      let array_cast = L.build_bitcast array_struct_malloc pointer_t "array_struct_cast" builder in
                      let _ = L.build_store array_cast head_ptr builder in ()
                  | A.Tuple (ty1, ty2) ->
                      let head_struct_malloc = L.build_malloc data_t "head_struct_malloc" builder in
                      let head_cast = L.build_pointercast head_struct_malloc pointer_t "head_cast" builder in
                      let _ = L.build_store head_cast head_ptr builder in
                      let tail_struct_malloc = L.build_malloc data_t "tail_struct_malloc" builder in
                      let tail_cast = L.build_pointercast tail_struct_malloc pointer_t "tail_cast" builder in
                      let _ = L.build_store tail_cast tail_ptr builder in
                      let _ = default_tuple head_struct_malloc ty1 builder in
                      let _ = default_tuple tail_struct_malloc ty2 builder in ()
                  | ty ->
                      let head_malloc = L.build_malloc (lltype_of_typ ty) "head_malloc" builder and
                          head_alloca = L.build_alloca (L.pointer_type (lltype_of_typ ty)) "head_alloca" builder in
                      let llvalue = default_value_of builder ty in
                      let _ = L.build_store llvalue head_malloc builder in
                      let _ = L.build_store head_malloc head_alloca builder in
                      let head_load = L.build_load head_alloca "head_load" builder in
                      let head_cast = cast_llvalue_to_ptr ty head_load builder in
                      let _ = L.build_store head_cast head_ptr builder in ()
              and default_array ptr typ builder =
                (match typ with
                  | A.Array (ty, len) ->
                      let rec build_identifier = function
                        | A.Tuple (t1, t2) -> 4 :: (build_identifier t1) @ (build_identifier t2)
                        | A.Array (base_t, length) -> [5; length] @ (build_identifier base_t)
                        | typ -> ocaml_tag typ in

                      let ocaml_tags = build_identifier typ in
                      let { size = size_ptr; data_array = data_ptr; tags = tags_ptr; tags_size = tags_size_ptr }
                        = build_array_gep ptr builder in
                      let _ = L.build_store (L.const_int i32_t len) size_ptr builder in
                      let tags_size = L.const_int i32_t (List.length ocaml_tags) in
                      let _ = L.build_store tags_size tags_size_ptr builder in
                      let tags_array = L.build_array_malloc i32_t tags_size "tags_array" builder in
                      let _ = List.mapi (fun i tag ->
                        let ptr = (L.build_in_bounds_gep tags_array [| L.const_int i32_t i |] "tags_gep" builder)  in
                        L.build_store (L.const_int i32_t tag) ptr builder
                      ) ocaml_tags in
                      let _ = L.build_store tags_array tags_ptr builder in
                      let array_malloc = L.build_array_malloc (lltype_of_typ ty) (L.const_int i32_t len) "array_malloc" builder in
                      let array_cast = L.build_pointercast array_malloc pointer_t "array_cast" builder in
                      let _ = L.build_store array_cast data_ptr builder in
                      (match ty with
                        | A.Array _  as ty ->
                            for i = 0 to len - 1 do
                              let gep = L.build_in_bounds_gep array_malloc [| L.const_int i32_t i |] ("array_lit_gep " ^ string_of_int i) builder in
                              let array_struct_ptr = L.build_malloc array_t "array_lit_alloca" builder in
                              let _ = default_array array_struct_ptr ty builder in
                              ignore (L.build_store array_struct_ptr gep builder)
                            done
                        | A.Tuple _ as ty ->
                            let data_ptr = L.build_malloc data_t "data_malloc" builder in
                            let _ = default_tuple data_ptr ty builder in
                            for i = 0 to len - 1 do
                              let gep = L.build_in_bounds_gep array_malloc [| L.const_int i32_t i |] ("array_lit_gep " ^ string_of_int i) builder in
                              ignore (L.build_store data_ptr gep builder)
                            done
                        | _ -> ())
                  | _ -> ()) in
              (match (ty, sx) with
                | (Array _, (_, SNoexpr)) ->
                    let array_malloc = L.build_malloc array_t "array_malloc" builder in
                    let _ = default_array array_malloc ty builder in array_malloc
                | (Tuple _, (_, SNoexpr)) ->
                    let tuple_malloc = L.build_malloc data_t "tuple_malloc" builder in
                    let _ = default_tuple tuple_malloc ty builder in tuple_malloc
                | _ -> L.build_bitcast llvalue (lltype_of_typ ty) "cast_initialize" builder) in

            let _ = L.build_store llvalue alloca builder in (builder, StringMap.add var_name alloca env2)
        | SDecl (STupleDecl (_, _, sexpr) as tupDecl) ->
            let rec unpack_tuple decl_type tuple_data env =
              let { head = head_ptr; tail = tail_ptr; _ } = build_data_gep tuple_data builder in
              match decl_type with
              | SBaseDecl (base_ty, id, _) as base_decl ->
                  let (_, env') = stmt (builder, env, ctx) (SDecl base_decl) in
                  let storage = StringMap.find id env' in
                  let value_ptr = L.build_load head_ptr "value_ptr_load" builder in
                  let value =
                    match base_ty with
                      | Array _ -> L.build_pointercast value_ptr array_ptr "head_cast" builder
                      | _ ->
                        let vptr = L.build_bitcast value_ptr (L.pointer_type (lltype_of_typ base_ty)) "head_cast" builder in
                        L.build_load vptr "value_load" builder in
                  let _ = L.build_store value storage builder in
                  env'
              | STupleDecl (left_decl, right_decl, _) ->
                  let head_load = L.build_load head_ptr "head_load" builder in
                  let head_cast = L.build_bitcast head_load data_ptr "head_cast" builder in
                  let tail_load = L.build_load tail_ptr "tail_load" builder in
                  let tail_cast = L.build_bitcast tail_load data_ptr "tail_cast" builder in
                  let env' = unpack_tuple left_decl head_cast env in
                  let env'' = unpack_tuple right_decl tail_cast env' in
                  env''
            in
            let (ll_data, env') = expr (builder, env) sexpr in
            let env'' = unpack_tuple tupDecl ll_data env' in
            (builder, env'')
        | SFor (init, cond, afterthought, body) ->
            let pred_bb = L.append_block context "for_pred" the_thread in
            let pred_builder = L.builder_at_end context pred_bb in
            let body_bb = L.append_block context "for_body" the_thread in
            let body_builder = L.builder_at_end context body_bb in
            let afterthought_bb = L.append_block context "for_post_body" the_thread in
            let afterthought_builder = L.builder_at_end context afterthought_bb in
            let end_bb = L.append_block context "for_end" the_thread in
            let end_builder = L.builder_at_end context end_bb in

            (* 1. Init (and go into first predicate) *)
            let (_, env') = stmt (builder, env, { continue_target_block = None; break_target_block = None }) init in
            let _ = L.build_br pred_bb builder in
            (* 2. Predicate - either ends loop or goes into the loop body *)
            let (pred, _) = expr (pred_builder, env') cond in
            let _ = L.build_cond_br pred body_bb end_bb pred_builder in
            (* 3. Body - the continue target is the post_body_bb *)
            let (final_builder, _) = stmt (body_builder, env', { continue_target_block = Some afterthought_bb; break_target_block = Some end_bb }) body in
            (* 4. Afterthought - runs after the body *)
            let _ = L.build_br afterthought_bb final_builder in
            let (afterthought_final_builder, _) = stmt (afterthought_builder, env', { continue_target_block = None; break_target_block = None }) (SExpr afterthought) in
            (* 5. Go back to the predicate to maybe loop again *)
            let _ = L.build_br pred_bb afterthought_final_builder in
            (* 6. End *)
            (end_builder, env)
        | SWhile (pred_expr, body) ->
            let pred_bb = L.append_block context "while_pred" the_thread in
            let pred_builder = L.builder_at_end context pred_bb in
            let body_bb = L.append_block context "while_body" the_thread in
            let body_builder = L.builder_at_end context body_bb in
            let end_bb = (L.append_block context "while_end" the_thread) in
            let end_builder = L.builder_at_end context end_bb in
            let _ = L.build_br pred_bb builder in
            let (pred, _) = expr (pred_builder, env) pred_expr in
            let _ = L.build_cond_br pred body_bb end_bb pred_builder in
            let (final_builder, _) = stmt (body_builder, env, { continue_target_block = Some body_bb; break_target_block = Some end_bb }) body in
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
            let (t_builder, _) = stmt (t_builder, env, ctx) t_block in
            let _ = L.build_br end_bb t_builder in
            let (f_builder, _) = stmt (f_builder, env, ctx) f_block in
            let _ = L.build_br end_bb f_builder in
            (end_builder, env)
        | SSend (lexpr, sexpr) ->
            let (typ, _) = sexpr in
            let (receiver_queue, env) = expr (builder, env) lexpr in
            let (llvalue, env') = expr (builder, env) sexpr in
            let data = (match typ with
                A.Tuple _ -> llvalue
              | A.Array _ ->
                  let data_alloca = L.build_alloca data_ptr "data_alloca" builder and
                      data_malloc = L.build_malloc data_t "data_malloc" builder in

                  let _ = L.build_store data_malloc data_alloca builder in
                  let data = L.build_load data_alloca "data_load" builder in
                  let { tag = tag_ptr; head = head_ptr; tail = tail_ptr } =
                    build_data_gep data builder in

                  let array_cast = L.build_pointercast llvalue pointer_t "array_cast" builder in
                  let _ = L.build_store (tag_of_type typ) tag_ptr builder in
                  let _ = L.build_store array_cast head_ptr builder in
                  let _ = L.build_store (L.const_null pointer_t) tail_ptr builder in data
              | _ ->
                let data_alloca = L.build_alloca data_ptr "data_alloca" builder and
                    data_malloc = L.build_malloc data_t "data_malloc" builder and
                    head_alloca = L.build_alloca (L.pointer_type (lltype_of_typ typ)) "head_alloca" builder and
                    head_malloc = L.build_malloc (lltype_of_typ typ) "head_malloc" builder and
                    tail_alloca = L.build_alloca (L.pointer_type (lltype_of_typ typ)) "tail_alloca" builder in

                let _ = L.build_store data_malloc data_alloca builder and
                    _ = L.build_store head_malloc head_alloca builder in

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
                let _ = L.build_store tail_cast tail_ptr builder in data)
              in let _ = L.build_call queue_push_func [| receiver_queue; data |] "" builder in (builder, env')
        | SContinue ->
            let { continue_target_block = continue_option; _ } = ctx in
            let _ = match continue_option with
                Some continue_bb -> L.build_br continue_bb builder
              | None -> raise (Failure "Found a continue statement without basic block") in
            (builder, env)
        | SBreak ->
            let { break_target_block = break_option; _ } = ctx in
            let _ = match break_option with
                Some break_bb -> L.build_br break_bb builder
              | None -> raise (Failure "Found a break statement without basic block") in
            (builder, env)
        | SReceive receive_cases ->
            (* Predicate block checks whether the queue is empty; used in the spin lock to wait for a message *)
            let pred_bb = L.append_block context "pred_bb" the_thread in
            let pred_builder = L.builder_at_end context pred_bb in
            (* Receive block is where we match each pattern against the data, to find which case we
             * need to jump to *)
            let receive_bb = L.append_block context "receive_bb" the_thread in
            let receive_builder = L.builder_at_end context receive_bb in
            (* “Find case” block finds the right index to switch on *)
            let find_case_bb = L.append_block context "find_case_bb" the_thread in
            let find_case_builder = L.builder_at_end context find_case_bb in
            (* Switch block does the switching to direct the program to the right case *)
            let switch_bb = L.append_block context "switch_bb" the_thread in
            let switch_builder = L.builder_at_end context switch_bb in
            (* End block directs to code after the receive block *)
            let end_bb = L.append_block context "end" the_thread in
            let end_builder = L.builder_at_end context end_bb in

            let { child_queue = receive_queue_ptr ; _ } = (match arg_gep with
                Some gep -> gep
              | None -> raise (Failure "TODO"))
            in

            (* First, busy wait for queue to be non-empty *)
            let receive_queue = L.build_load receive_queue_ptr "queue_load" builder in
            let _ = L.build_br pred_bb builder in
            let pred = L.build_call queue_empty_func [| receive_queue |] "pred" pred_builder in
            let _ = L.build_cond_br pred pred_bb receive_bb pred_builder in

            (* Generate the “tag array” for each of the cases in the receive block *)
            let ocaml_ptags = List.map (fun (pattern, _) -> tag_pattern pattern) receive_cases in
            let ptags_alloca = L.build_array_alloca (L.pointer_type i32_t)
                                (L.const_int i32_t (List.length ocaml_ptags)) "ptags_alloca" receive_builder in
            let lengths_alloca = L.build_array_alloca i32_t (L.const_int i32_t (List.length ocaml_ptags)) "lengths_alloca" receive_builder in
            (* let ptags = L.build_load ptags_alloca "ptag_load" receive_builder in *)
            (* Convert OCaml tag to its LLVM representation *)
            (* For each tag in our list of receieve cases... *)
            let _ = List.iteri (fun i ptag ->
              (* let _ = L.build_call printf_func [| L.build_global_stringptr "pattern #%d:\n  " "fmt" receive_builder; (L.const_int i32_t i) |] "print_test" receive_builder in *)
              (* Store the length of the ptag *)
              let length_ptr = L.build_in_bounds_gep lengths_alloca [| L.const_int i32_t i |] "lengths_gep" receive_builder in
              let _ = L.build_store (L.const_int i32_t (List.length ptag)) length_ptr receive_builder in
              (* Pointer to this slot in our array of tags, which will hold an array of integers *)
              let ptag_ptr = L.build_in_bounds_gep ptags_alloca [| L.const_int i32_t i |] "ptags_gep" receive_builder in
              (* Create the array of integers to be the tag *)
              let ptag_alloca = L.build_array_alloca i32_t (L.const_int i32_t (List.length ptag)) "ptag_alloca" receive_builder in
              let _ = L.build_store ptag_alloca ptag_ptr receive_builder in
              let _ = List.iteri (fun j elem ->
                let tag_elem_ptr = L.build_in_bounds_gep ptag_alloca [| L.const_int i32_t j |] "ptr_gep" receive_builder in
                let _ = L.build_store (L.const_int i32_t elem) tag_elem_ptr receive_builder in
                (* Uncomment below to see printed tags *)
                (* let _ = L.build_call printf_func [| L.build_global_stringptr "%d " "fmt" receive_builder; (L.build_load tag_elem_ptr "tag_elem_load" receive_builder) |] "print_test" receive_builder in *)
                ())
                ptag
              (* in let _ = L.build_call printf_func [| L.build_global_stringptr "\n" "fmt" receive_builder; (L.const_int i32_t 0) |] "newline" receive_builder *)
              in ()) ocaml_ptags in


            (* Pop the message off the queue *)
            let message_data_ptr = L.build_call queue_pop_func [| receive_queue |] "queue_pop" receive_builder in

            (* Find the case to jump to *)

            (* Stores the index of the first case case that matched *)
            let case_index = L.build_alloca i32_t "index" receive_builder in
            let _ = L.build_store (L.const_int i32_t (-1)) case_index receive_builder in

            (* “Find case” block finds the right index to switch on *)
            (* go into the find_case_bb *)
            let _ = L.build_br find_case_bb receive_builder in
            (* Increment the index to start examining the next case *)
            let old_index_loaded = L.build_load case_index "index_load" find_case_builder in
            let case_index_increment = L.build_add old_index_loaded (L.const_int i32_t 1) "index_increment" find_case_builder in
            let _ = L.build_store case_index_increment case_index find_case_builder in
            let index_loaded = L.build_load case_index "index_load" find_case_builder in
            (* Set up everything we need to pass to tag_compare_func *)
            let case_tag_ptr = L.build_in_bounds_gep ptags_alloca [| index_loaded |] "tag_ptr" find_case_builder in
            let case_tag = L.build_load case_tag_ptr "tag_load" find_case_builder in
            let case_tag_index_alloca = L.build_alloca i32_t "tag_index_ptr" find_case_builder in
            let _ = L.build_store (L.const_int i32_t 0) case_tag_index_alloca find_case_builder in
            let case_tag_length_ptr = L.build_in_bounds_gep lengths_alloca [| index_loaded |] "length" find_case_builder in
            let case_tag_length = L.build_load case_tag_length_ptr "length_load" find_case_builder in
            (* Did this case match? *)

            (* let debug = L.build_global_stringptr "Made it here\n" "debug" find_case_builder in
            let _ = L.build_call printf_func [| string_format_str; debug |] "call" find_case_builder in *)
            let case_matched = L.build_call tag_compare_func [| case_tag; case_tag_index_alloca; case_tag_length; message_data_ptr |] "tag_comparison" find_case_builder in
            (* If it did, jump to the switch_bb. If not, we go back to find_case_bb to check the next case. *)
            let _ = L.build_cond_br case_matched switch_bb find_case_bb find_case_builder in

            (* Switch block directs the program to the proper case once the matching case_index has been identified *)
            let index_loaded = L.build_load case_index "index_load" switch_builder in

            (* the “else” case of the switch block should never be reached because semantic checker should enforce that there is a wildcard *)
            let else_bb = L.append_block context "default" the_thread in
            let else_builder  = L.builder_at_end context else_bb in
            let _ = L.build_call printf_func [| L.build_global_stringptr "no patterns matched; this should never happen" "test" builder; (L.const_int i32_t (-1)) |] "print_test" else_builder in
            let _ = L.build_br end_bb else_builder in

            let switch = L.build_switch index_loaded else_bb (List.length receive_cases) switch_builder in

            let rec extend_env data_ptr_o builder env = function
              | SBasePattern (A.Array _, id ) ->
                let { head = head_ptr; _ } = build_data_gep data_ptr_o builder in
                let head_load = L.build_load head_ptr "head_load" builder in
                let head_cast = L.build_pointercast head_load array_ptr "head_cast" builder in
                let array_alloca = L.build_alloca array_ptr "array_alloca" builder in
                let _ = L.build_store head_cast array_alloca builder in
                StringMap.add id array_alloca env
              | SBasePattern (typ, id) ->
                let { head = head_ptr; _ } = build_data_gep data_ptr_o builder in
                let value_alloca = L.build_alloca (lltype_of_typ typ) "value_alloca" builder in
                let value_ptr = L.build_load head_ptr "value_ptr_load" builder in
                let value_ptr = L.build_bitcast value_ptr (L.pointer_type (lltype_of_typ typ)) "value_ptr_cast" builder in
                let value = L.build_load value_ptr "value_load" builder in
                let _ = L.build_store value value_alloca builder in
                StringMap.add id value_alloca env
              | SWildcardPattern -> env
              | STuplePattern (p1, p2) ->
                  let { head = head_ptr; tail = tail_ptr; _ } = build_data_gep data_ptr_o builder in
                  let head_load = L.build_load head_ptr "head_load" builder in
                  let head_cast = L.build_bitcast head_load data_ptr "head_cast" builder in
                  let tail_load = L.build_load tail_ptr "tail_load" builder in
                  let tail_cast = L.build_bitcast tail_load data_ptr "tail_cast" builder in
                  let env' = extend_env head_cast builder env p1 in extend_env tail_cast builder env' p2
            in

            (* Build basic blocks for the body of each pattern *)
            let _ = List.iteri
              (fun i receive_case ->
                let case_bb = L.append_block context "receive_case" the_thread in
                let case_builder = L.builder_at_end context case_bb in
                let _ = (L.add_case switch (L.const_int i32_t i) case_bb) in
                let (pattern, sstmt) = receive_case in
                let env' = extend_env message_data_ptr case_builder env pattern in
                let (new_builder, _) = stmt (case_builder, env', ctx) sstmt in
                ignore (L.build_br end_bb new_builder )) receive_cases in

            (* Check if tags match and if so, pass that index to the switch block *)
            (end_builder, env) in
    stmt (builder, env, { continue_target_block = None; break_target_block = None}) sstmt in

  let build_thread_body tdecl =
    let (the_thread, _) = StringMap.find tdecl.stname thread_decls in
    let builder = L.builder_at_end context (L.entry_block the_thread) in
    let argument = L.build_bitcast (L.param the_thread 0) arg_ptr "cast_void" builder in
    let arg_gep = build_arg_gep argument builder in
    let { parent_queue = parent_queue_ptr; child_queue = child_queue_ptr; _ } = arg_gep in
    let parent_queue_alloca = L.build_alloca queue_ptr "parent_queue_alloca" builder in
    let child_queue_alloca = L.build_alloca queue_ptr "child_queue_alloca" builder in
    let parent_queue = L.build_load parent_queue_ptr "parent_queue_load" builder in
    let child_queue = L.build_load child_queue_ptr "child_queue_load" builder in
    let _ = L.build_store parent_queue parent_queue_alloca builder in
    let _ = L.build_store child_queue child_queue_alloca builder in
    let env = StringMap.empty in
    let env = StringMap.add "parent" parent_queue_alloca env in
    let env = StringMap.add "self" child_queue_alloca env in
    let (final_builder, _) =
      build_body ~arg_gep:arg_gep (builder, env) (SBlock tdecl.sbody) the_thread in
    (* thread function follows pthread function type and returns a NULL pointer *)
    let global_mutex_loaded = L.build_load global_mutex "global_mutex_load" final_builder in
    let _ = L.build_call mutex_lock_func [| global_mutex_loaded |] "" final_builder in
    let global_counter_load = L.build_load global_counter "global_counter_load" final_builder in
    let global_counter_load = L.build_sub global_counter_load (L.const_int i64_t 1) "sub_global_counter" final_builder in
    let _ = L.build_store global_counter_load global_counter final_builder in
    let _ = L.build_call mutex_unlock_func [| global_mutex_loaded |] "" final_builder in
    add_terminal final_builder (L.build_ret (L.const_null pointer_t))
  and build_func_body fdecl =
    let (the_func, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_func) in
    let env = List.fold_left2 (fun map (typ, id) param ->
        let local = L.build_alloca (lltype_of_typ typ) (id ^ "_alloca") builder in
        let _ = L.build_store param local builder in StringMap.add id local map
      ) StringMap.empty fdecl.sformals (Array.to_list (L.params the_func)) in
    let _ = build_body (builder, env) (SBlock fdecl.sbody) the_func in
    () in
  let _ = List.map build_thread_body tdecls in
  let _ = List.map build_func_body fdecls in

  let main_t = L.function_type i32_t [| |] in
  let main_func = L.define_function "main" main_t the_module in
    let builder = L.builder_at_end context (L.entry_block main_func) in

    let wait_bb = L.append_block context "wait_bb" main_func in
    let done_bb = L.append_block context "done_bb" main_func in
    let wait_builder = L.builder_at_end context wait_bb in
    let done_builder = L.builder_at_end context done_bb in

    (* Initialize mutex *)
    let _ = L.build_call mutex_init_func [| global_mutex |] "" builder in

    let (main_thread, _) = StringMap.find "Main" thread_decls in

    let arg_malloc = L.build_malloc arg_t "arg_malloc" builder in
    let parent_queue = L.build_call queue_init_func [| |] "init_parent_queue" builder in

    let { parent_queue = parent_queue_ptr; child_queue = child_queue_ptr }
      = build_arg_gep arg_malloc builder in
    let _ = L.build_store parent_queue parent_queue_ptr builder in
    let _ = L.build_store parent_queue child_queue_ptr builder in

    let arg = L.build_bitcast arg_malloc pointer_t "cast_arg" builder in
    let _ = L.build_call main_thread [| arg |] "star_main_thread" builder in
    let _ = L.build_br wait_bb builder in

    let _ = L.build_br wait_bb builder in
    let global_counter_loaded = L.build_load global_counter "global_counter_loaded" wait_builder in
    let finished = L.build_icmp L.Icmp.Eq global_counter_loaded (L.const_int i64_t 0) "done" wait_builder in
    let _ = L.build_cond_br finished done_bb wait_bb wait_builder in

    let _ = add_terminal done_builder (L.build_ret (L.const_int i32_t 0)) in the_module
