(* We'll refer to Llvm and Ast constructs with module names *)
module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (globals, functions) =
  let context    = L.global_context () in
  (* Add types to the context so we can use them in our LLVM code *)
  let i32_t      = L.i32_type          context (*int*)
  and i8_t       = L.i8_type           context (*char/string*)
  and i1_t       = L.i1_type           context (*bool*)
  and void_t     = L.void_type         context (*void*)
  (*and p_t  = L.pointer_type (L.i8_type (context))*)(*pointer type*)
  let string_pointer = L.pointer_type (L.i8_type context) in
  let rec map_t = L.struct_type context [| string_pointer; i32_t ; map_ptr_t |] 
  and map_ptr_t = L.pointer_type map_t
  let player_t = L.struct_type context [| i32_t ; i1_t ; string_pointer ; map_t  |]

  (*and cnode_t    = L.struct_type       context [| i8_t; (p_t cnode_t) |]*) (*char node*)
  (*@TODO: add structs/other stuff*)
  (* Create an LLVM module -- this is a "container" into which we'll
     generate actual code *)
  and the_module = L.create_module context "Olleh" in

  (* Convert Olleh types to LLVM types *)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Char  -> i8_t
    | A.String -> (*L.pointer_type*) i8_t (*???*)
    | A.Void  -> void_t
    | A.Map -> map_ptr_t
    | A.Player -> player_t
    | A.List -> string_pointer (* @TODO adapt for 2D *)
    | _ -> void_t (*temp; @TODO: add for structs*)
  in

  (* Declare each global variable; remember its value in a map *)

  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = match t with
          _ -> L.const_int (ltype_of_typ t) 0 (*@TODO: IMPLEMENT*)
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (*builtins: print*)
  let printf_t : L.lltype =
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
     L.declare_function "printf" printf_t the_module in

  (*builtins: getLength of a string and convert int to string*)
  (*let intstr_t : L.lltype =
      L.function_type i32_t [| L.pointer_type i8_t |] in
  let getLength_func : L.llvalue =
     L.declare_function "strlen" intstr_t the_module in*)
  let strint_t : L.lltype =
      L.function_type (L.pointer_type i8_t) [| i32_t |] in
  let intToString_func : L.llvalue =
     L.declare_function "IntToS" strint_t the_module in

  (*builtins: random*)
  (*let intvoid_t : L.lltype =
      L.function_type i32_t [| |] in
  let rand_funct : L.llvalue =
     L.declare_function "OllehRandom" intvoid_t the_module in*)

  (*builtins: functions the user cannot explicitly call*)
  let voidvoid_t : L.lltype =
      L.function_type void_t [| |] in
  (*let garbagei_func : L.llvalue =
      L.declare_function "InitializeLocalGarbage" voidvoid_t the_module in
  let garbagec_func : L.llvalue =
      L.declare_function "CollectLocalGarbage" voidvoid_t the_module in*)
  let randi_func : L.llvalue =
      L.declare_function "InitializeRandom" voidvoid_t the_module in

  (*let voidchar_t : L.lltype =
      L.function_type void_t [| L.pointer_type i8_t |] in
  let garbagecc_func : L.llvalue =
      L.declare_function "CollectLocalGarbageWithReturn" voidchar_t the_module in*)

  let charcharchar_t : L.lltype =
      L.function_type (L.pointer_type i8_t) [| L.pointer_type i8_t; L.pointer_type i8_t |] in
  let strcati_func : L.llvalue =
      L.declare_function "SConcat" charcharchar_t the_module in

  let strintlis_t : L.lltype =
     L.function_type void_t [| L.pointer_type i8_t |] in
  let printil_func : L.llvalue =
     L.declare_function "PrintCharLis" strintlis_t the_module in

  (* Define each function (arguments and return type) so we can
   * define it's body and call it later *)
   let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
     let function_decl m fdecl =
       let name = fdecl.sfname
       and formal_types =
 	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
       in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
       StringMap.add name (L.define_function name ftype the_module, fdecl) m in
     List.fold_left function_decl StringMap.empty functions in

   (* Fill in the body of the given function *)
   let build_function_body fdecl =
     let (the_function, _) = StringMap.find fdecl.sfname function_decls in
     let builder = L.builder_at_end context (L.entry_block the_function) in

     let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
     let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

     (* Construct the function's "locals": formal arguments and locally
        declared variables.  Allocate each on the stack, initialize their
        value, if appropriate, and remember their values in the "locals" map *)

    let local_vars =
      let add_formal m (t, n) p =
        let () = L.set_value_name n p in
	let local = L.build_alloca (ltype_of_typ t) n builder in
        let _  = L.build_store p local builder in
	StringMap.add n local m
      in

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      let add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals [] (*@TODO: ADD LATERfdecl.slocals*)
    in

    (* Return the value for a variable or formal argument. First check
     * locals, then globals *)

    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    let sentinel = Int32.to_int (Int32.div Int32.max_int (Int32.of_int 2)) in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
	SLiterali i -> L.const_int i32_t i
      | SLiteralc c -> L.const_int i8_t (int_of_char c)
      | SLiteralb b -> L.const_int i1_t (if b then 1 else 0)
      | SLiterals s -> L.build_global_stringptr s "" builder
      | SNoexpr -> L.const_int i32_t 0
      | Null -> L.const_int i32_t 0
      | SVariable s -> L.build_load (lookup s) s builder
      | SVmember (v, _) -> L.build_load (lookup v) v builder (*@TODO: IMPLEMENT*)
      | SLiterall (t, l) ->
         let l' = List.map (expr builder) l in
         let len = (List.length l) + 1 in
         if t = Int then
           let y = L.build_array_malloc i32_t (L.const_int i32_t len) "a1" builder
           in let x = L.build_pointercast y (L.pointer_type i32_t) "a2" builder
           in let addtoar index elem =
             let xx = L.build_gep x [| L.const_int i32_t index |] "a3" builder
             in ignore (L.build_store elem xx builder)
           in let _ = List.iteri addtoar l'
           in let _ = addtoar (len - 1) (L.const_int i32_t sentinel)
           in x
         else if t = Char then
           let y = L.build_array_malloc i8_t (L.const_int i32_t len) "a1" builder
           in let x = L.build_pointercast y (L.pointer_type i8_t) "a2" builder
           in let addtoar index elem =
             let xx = L.build_gep x [| L.const_int i32_t index |] "a3" builder
             in ignore (L.build_store elem xx builder)
           in let _ = List.iteri addtoar l'
           in let _ = addtoar (len - 1) (L.const_int i8_t 0) (*sentinel*)
         in x
         else raise (Failure "build error")
      | SLiteralm _ -> L.const_int i32_t 0 (*@TODO: IMPLEMENT*)
      | SAssignm (s, _, e) -> let e' = expr builder e in
                          let _  = L.build_store e' (lookup s) builder in e' (*@TODO: IMPLEMENT/UNDERSTAND*)
      | SAssign (s, e) -> let e' = expr builder e in
                          let _  = L.build_store e' (lookup s) builder in e' (*@TODO: IMPLEMENT/UNDERSTAND*)
      | SBinop (e1, op, e2) ->
	  let (t, _) = e1 and (tt, _) = e2
	  and e1' = expr builder e1
	  and e2' = expr builder e2 in
	  if t = A.Int then (match op with
          | A.Add     -> L.build_add
	  | A.Sub     -> L.build_sub
	  | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
	  | A.And     -> L.build_and
	  | A.Or      -> L.build_or
          | A.Mod     -> L.build_urem
	  | A.Equal   -> L.build_icmp L.Icmp.Eq
	  | A.Neq     -> L.build_icmp L.Icmp.Ne
	  | A.Less    -> L.build_icmp L.Icmp.Slt
	  | A.Leq     -> L.build_icmp L.Icmp.Sle
	  | A.Greater -> L.build_icmp L.Icmp.Sgt
	  | A.Geq     -> L.build_icmp L.Icmp.Sge
	      ) e1' e2' "tmp" builder
          else if t = A.String then
            if op = A.Add then
              if tt = A.Int then
                let resultt = "strcatt_result" in
                  L.build_call strcati_func [| e1'; (let r2 = "ITOS_result" in
                    L.build_call intToString_func [| e2' |] r2 builder) |] resultt builder
              else
            let result = "strcat_result" in
              L.build_call strcati_func [| e1'; e2' |]
              result builder
          else raise (Failure "internal error: not binop for string")
          else raise (Failure "internal error: binop for non int not (yet) implemented")
          (*@TODO: IMPLEMENT FURTHER*)
      | SUnop(op, e) ->
	  let _ = e in
          let e' = expr builder e in
	  (match op with
            A.Neg                  -> L.build_neg
          | A.Not                  -> L.build_not) e' "tmp" builder
      (*| SCall("nameOfBuiltin") ...: implement for our builtins*)
      | SNewtobj _ -> L.const_int i32_t 0 (*@TODO: IMPLEMENT*)
      | SNewobj _ -> L.const_int i32_t 0 (*@TODO: IMPLEMENT*)
      (*| SCall ("InitializeLocalGarbage",[]) ->
        L.build_call garbagei_func [| |]
        "" builder
      | SCall ("CollectLocalGarbage",[]) ->
         L.build_call garbagec_func [| |]
         "" builder*)
      | SCall ("InitializeRandom",[]) ->
         L.build_call randi_func [| |]
         "" builder
      | SCall (f, args) ->
         let (fdef, fdecl) = try StringMap.find f function_decls with Not_found -> raise (Failure ("FAIL " ^ f)) in
	 let llargs = List.rev (List.map (expr builder) (List.rev args)) in
	 let result = (match fdecl.styp with
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder
      | SCallm (_, f, args) -> (*@TODO: IMPLEMENT *)
            let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (expr builder) (List.rev args)) in
        let result = (match fdecl.styp with
                           A.Void -> ""
                         | _ -> f ^ "_result") in
            L.build_call fdef (Array.of_list llargs) result builder
    in

    (* Each basic block in a program ends with a "terminator" instruction i.e.
    one that ends the basic block. By definition, these instructions must
    indicate which basic block comes next -- they typically yield "void" value
    and produce control flow, not values *)
    (* Invoke "instr builder" if the current block doesn't already
       have a terminator (e.g., a branch). *)
    let add_terminal builder instr =
                           (* The current block where we're inserting instr *)
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (instr builder) in



    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    (* Imperative nature of statement processing entails imperative OCaml *)
    let rec stmt builder = function
        SExpr e -> let _ = expr builder e in builder
      | SPrint (t, e) ->
          if t = String then let _ = L.build_call printf_func [| str_format_str ; (expr builder (t,e)) |]
	    "printf" builder in builder
         else if t = List then
           let ex = expr builder (t, e) in
           (*let n = 4 (L.array_length (L.type_of ex)) in*)
           let _ = L.build_call printil_func [| ex |]
            "" builder in builder
         else if t = Int then let _ = L.build_call printf_func [| int_format_str ; (expr builder (t,e)) |]
           "printf" builder in builder
          else raise (Failure "internal error: print not (yet) implemented")
      (* The order that we create and add the basic blocks for an If statement
      doesnt 'really' matter (seemingly). What hooks them up in the right order
      are the build_br functions used at the end of the then and else blocks (if
      they don't already have a terminator) and the build_cond_br function at
      the end, which adds jump instructions to the "then" and "else" basic blocks *)
      | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
         (* Add "merge" basic block to our function's list of blocks *)
	 let merge_bb = L.append_block context "merge" the_function in
         (* Partial function used to generate branch to merge block *)
         let branch_instr = L.build_br merge_bb in

         (* Same for "then" basic block *)
	 let then_bb = L.append_block context "then" the_function in
         (* Position builder in "then" block and build the statement *)

         let then_builder = List.fold_left stmt (L.builder_at_end context then_bb) then_stmt in
         (* Add a branch to the "then" block (to the merge block)
           if a terminator doesn't already exist for the "then" block *)
	 let () = add_terminal then_builder branch_instr in

         (* Identical to stuff we did for "then" *)
	 let else_bb = L.append_block context "else" the_function in
         let else_builder = List.fold_left stmt (L.builder_at_end context else_bb) else_stmt in
	 let () = add_terminal else_builder branch_instr in

         (* Generate initial branch instruction perform the selection of "then"
         or "else". Note we're using the builder we had access to at the start
         of this alternative. *)
	 let _ = L.build_cond_br bool_val then_bb else_bb builder in
         (* Move to the merge block for further instruction building *)
	 L.builder_at_end context merge_bb
     (*| SBind(t, s) ->
       let add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m
       in let local_vars = add_local local_vars (t, s)*)
     | _ -> raise (Failure "internal error: not (yet) implemented")
     (*| SReturn e -> let _ = match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder
                     in builder
      | SWhile (predicate, body) ->
          (* First create basic block for condition instructions -- this will
          serve as destination in the case of a loop *)
	  let pred_bb = L.append_block context "while" the_function in
          (* In current block, branch to predicate to execute the condition *)
	  let _ = L.build_br pred_bb builder in

          (* Create the body's block, generate the code for it, and add a branch
          back to the predicate block (we always jump back at the end of a while
          loop's body, unless we returned or something) *)
	  let body_bb = L.append_block context "while_body" the_function in
          let while_builder = stmt (L.builder_at_end context body_bb) body in
	  let () = add_terminal while_builder (L.build_br pred_bb) in

          (* Generate the predicate code in the predicate block *)
	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

          (* Hook everything up *)
	  let merge_bb = L.append_block context "merge" the_function in
	  let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
	  L.builder_at_end context merge_bb

      (* Implement for loops as while loops! *)
      | SFor (e1, e2, e3, body) -> stmt builder
	    ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
            @TODO: IMPLEMENT ALL OF THIS COMMENT BLOCK*)
    in

    (* Build the code for each statement in the function *)
    let builder = List.fold_left stmt builder fdecl.sbody
     in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | _ -> raise (Failure "internal error: return type not (yet) implemented"))
      (*| A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0)) @TODO: IMPLEMENT*)
  in

  List.iter build_function_body functions;
  the_module
