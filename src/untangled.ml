type action = Ast | Sast | LLVM_IR

let () =
  let action = ref LLVM_IR in
  let set_action a () = action := a in
  let output_file = ref "" in
  let speclist = [
    ("--print-ast", Arg.Unit (set_action Ast), "Print the AST");
    ("--print-sast", Arg.Unit (set_action Sast), "Print the SAST");
    ("--print-llvm", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-o", Arg.Set_string output_file, "Compile to an executable with the given name. When provided, supersedes other output options; no output is printed to the console.");
  ] in
  let usage_msg = "usage: ./untangled.native [-a|-s|-l|-c] [file.unt]" in
  let channel = ref stdin in
    Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  match !action with
    Ast -> print_string (Ast.string_of_program ast)
    | _ ->
      let sast = Semant.check ast in
        if !output_file = "" then
          match !action with
            Ast -> ()
            | Sast    -> print_string (Sast.string_of_sprogram sast)
            | LLVM_IR -> let m = Codegen.translate sast in
                          Llvm_analysis.assert_valid_module m;
                          print_string (Llvm.string_of_llmodule m)
        else
          let exe_path = !output_file in
          let ll_path = exe_path ^ ".ll" in
          let s_path = exe_path ^ ".s" in
          let ll_file = open_out ll_path in
          Printf.fprintf ll_file "%s" (Llvm.string_of_llmodule (Codegen.translate sast));
          close_out ll_file;
          ignore (Sys.command ("llc " ^ ll_path ^ " -o " ^ s_path));
          ignore (Sys.command ("clang -O0 src/c/libraries.o " ^ s_path ^ " -o " ^ exe_path));
