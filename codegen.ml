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
  in let string_pointer = L.pointer_type (L.i8_type context) in
  let listlist_ptr = L.pointer_type string_pointer in
  let map_t = L.named_struct_type context "mapt" in
  let map_ptr_t = L.pointer_type map_t in
  let _ = L.struct_set_body map_t [| string_pointer; i32_t; map_ptr_t |] false in
  let charmap_t = L.named_struct_type context "charmapt" in
  let cmap_ptr_t = L.pointer_type charmap_t in
  let _ = L.struct_set_body charmap_t [| i8_t; i32_t; cmap_ptr_t |] false in
  let player_t = L.named_struct_type context "playert" in
  let player_ptr_t = L.pointer_type player_t in
  let _ = L.struct_set_body player_t [| i32_t ; i1_t ; string_pointer ; map_t |] false

  (* Create an LLVM module -- this is a "container" into which we'll
     generate actual code *)
  and the_module = L.create_module context "Olleh" in

  (* Convert Olleh types to LLVM types *)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Char  -> i8_t
    | A.String -> string_pointer
    | A.Void  -> void_t
    | A.Stringmap -> map_ptr_t
    | A.Charmap -> cmap_ptr_t (*@TODO: possibly switch to separate struct*)
    | A.Player -> player_ptr_t
    | A.Charlist -> string_pointer (* @TODO adapt for 2D *)
    | A.Listlist -> listlist_ptr
  in

  (* Declare each global variable; remember its value in a map *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = match t with
            A.Int -> L.const_int (ltype_of_typ t) 0
          | A.Bool -> L.const_int (ltype_of_typ t) 0
          | A.Char -> L.const_int (ltype_of_typ t) 0
          | _ -> L.const_null (ltype_of_typ t)
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (*builtins: printing and strings*)
  let printf_t : L.lltype =
      L.var_arg_function_type i32_t [| string_pointer |] in
  let printf_func : L.llvalue =
     L.declare_function "printf" printf_t the_module in
  let strint_t : L.lltype =
      L.function_type string_pointer [| i32_t |] in
  let intToString_func : L.llvalue =
     L.declare_function "IntToS" strint_t the_module in (*helper function for printing/concatenation*)
  let charcharchar_t : L.lltype =
      L.function_type string_pointer [| string_pointer; string_pointer |] in
  let strcati_func : L.llvalue =
      L.declare_function "SConcat" charcharchar_t the_module in
  let voidlis_t : L.lltype =
     L.function_type void_t [| string_pointer |] in
  let printil_func : L.llvalue =
     L.declare_function "PrintCharLis" voidlis_t the_module in
  let voidsmap_t : L.lltype =
     L.function_type void_t [| map_ptr_t |] in
  let printsmap_func : L.llvalue =
     L.declare_function "PrintStringmap" voidsmap_t the_module in
  let voidcmap_t : L.lltype =
     L.function_type void_t [| cmap_ptr_t |] in
  let printcmap_func : L.llvalue =
     L.declare_function "PrintCharmap" voidcmap_t the_module in
  let voidlislis_t : L.lltype =
     L.function_type void_t [| listlist_ptr |] in
  let printll_func : L.llvalue =
     L.declare_function "PrintListList" voidlislis_t the_module in

  (*builtins: object functions*)
  (*getLength*)
  let intstr_t : L.lltype =
      L.function_type i32_t [| string_pointer |] in
  let cllen_func : L.llvalue =
     L.declare_function "strlen" intstr_t the_module in
  let intlislis_t : L.lltype =
     L.function_type i32_t [| listlist_ptr |] in
  let lllen_func : L.llvalue =
     L.declare_function "ListlistgetLength" intlislis_t the_module in
  let intcmap_t : L.lltype =
     L.function_type i32_t [| cmap_ptr_t |] in
  let cmlen_func : L.llvalue =
     L.declare_function "CharmapgetLength" intcmap_t the_module in
  let intsmap_t : L.lltype =
     L.function_type i32_t [| map_ptr_t |] in
  let smlen_func : L.llvalue =
     L.declare_function "StringmapgetLength" intsmap_t the_module in
  (*getters*)
  let cmapchar_t : L.lltype =
     L.function_type cmap_ptr_t [| cmap_ptr_t; i8_t |] in
  let cmapget_func : L.llvalue =
     L.declare_function "Charmapget" cmapchar_t the_module in
  let smapchar_t : L.lltype =
     L.function_type map_ptr_t [| map_ptr_t; string_pointer |] in
  let smapget_func : L.llvalue =
     L.declare_function "Stringmapget" smapchar_t the_module in
  let charcmap_t : L.lltype =
     L.function_type i8_t [| cmap_ptr_t; i32_t |] in
  let cmapgeti_func : L.llvalue =
     L.declare_function "Charmapgeti" charcmap_t the_module in
  let strsmap_t : L.lltype =
     L.function_type string_pointer [| map_ptr_t; i32_t |] in
  let smapgeti_func : L.llvalue =
     L.declare_function "Stringmapgeti" strsmap_t the_module in
  let charclis_t : L.lltype =
     L.function_type i8_t [| string_pointer; i32_t |] in
  let clget_func : L.llvalue =
     L.declare_function "Charlistget" charclis_t the_module in
  let clslis_t : L.lltype =
     L.function_type string_pointer [| listlist_ptr; i32_t |] in
  let llget_func : L.llvalue =
     L.declare_function "Listlistget" clslis_t the_module in
  (*setters*)
  let cmapcharint_t : L.lltype =
     L.function_type cmap_ptr_t [| cmap_ptr_t; i8_t; i32_t |] in
  let cmapset_func : L.llvalue =
     L.declare_function "Charmapset" cmapcharint_t the_module in
  let smapcharint_t : L.lltype =
     L.function_type map_ptr_t [| map_ptr_t; string_pointer; i32_t |] in
  let smapset_func : L.llvalue =
     L.declare_function "Stringmapset" smapcharint_t the_module in
  let voidillislis_t : L.lltype =
     L.function_type void_t [| listlist_ptr; i32_t; string_pointer |] in
  let llset_func : L.llvalue =
     L.declare_function "Listlistset" voidillislis_t the_module in
  let voidicclis_t : L.lltype =
     L.function_type void_t [| string_pointer; i32_t; i8_t |] in
  let clset_func : L.llvalue =
     L.declare_function "Charlistset" voidicclis_t the_module in
  (*misc*)
  let cmapchar_t : L.lltype =
     L.function_type cmap_ptr_t [| cmap_ptr_t; i8_t |] in
  let cmapd_func : L.llvalue =
     L.declare_function "Charmapdestroy" cmapchar_t the_module in
  let smapstr_t : L.lltype =
     L.function_type map_ptr_t [| map_ptr_t; string_pointer |] in
  let smapd_func : L.llvalue =
     L.declare_function "Stringmapdestroy" smapstr_t the_module in
  let cmapcharc_t : L.lltype =
     L.function_type i8_t [| cmap_ptr_t; i8_t |] in
  let cmapc_func : L.llvalue =
     L.declare_function "Charmapcontains" cmapcharc_t the_module in
  let smapstrc_t : L.lltype =
     L.function_type i8_t [| map_ptr_t; string_pointer |] in
  let smapc_func : L.llvalue =
     L.declare_function "Stringmapcontains" smapstrc_t the_module in
  let voidllint_t : L.lltype =
     L.function_type void_t [| listlist_ptr; i32_t; i32_t |] in
  let fillislis_func : L.llvalue =
     L.declare_function "FillListlist" voidllint_t the_module in
  let voidlint_t : L.lltype =
     L.function_type void_t [| string_pointer; i32_t |] in
  let fillis_func : L.llvalue =
     L.declare_function "FillList" voidlint_t the_module in

  (*builtins: misc*)
  let voidint_t : L.lltype =
     L.function_type void_t [| i32_t |] in
  let exit_func : L.llvalue =
     L.declare_function "exit" voidint_t the_module in
  let charint_t : L.lltype =
     L.function_type i8_t [| i32_t |] in
  let ascii_func : L.llvalue =
     L.declare_function "ToAscii" charint_t the_module in
  let intvoid_t : L.lltype =
      L.function_type i32_t [| |] in
  let rand_funct : L.llvalue =
     L.declare_function "OllehRandom" intvoid_t the_module in
  let strvoid_t : L.lltype =
      L.function_type string_pointer [| |] in
  let rinput_funct : L.llvalue =
     L.declare_function "readInput" strvoid_t the_module in
  let smstr_t : L.lltype =
      L.function_type map_ptr_t [| string_pointer |] in
  let rdict_funct : L.llvalue =
     L.declare_function "readDictionary" smstr_t the_module in
  let voidvoid_t : L.lltype =
      L.function_type void_t [| |] in
  let randi_func : L.llvalue =
      L.declare_function "InitializeRandom" voidvoid_t the_module in
  let strstr_t : L.lltype =
      L.function_type string_pointer [| string_pointer |] in
  let anagram_func : L.llvalue =
     L.declare_function "anagram" strstr_t the_module in

  (* Define each function (arguments and return type) so we can
   * define it's body and call it later *)
   let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
     let function_decl m fdecl =
       let name = fdecl.sfname
       and formal_types =
 	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
        (*in let _ = List.map (fun x -> print_string (A.string_of_typ (fst x))) fdecl.sformals*)
       in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
       StringMap.add name (L.define_function name ftype the_module, fdecl) m in
     List.fold_left function_decl StringMap.empty functions in

   (* Fill in the body of the given function *)
   let build_function_body fdecl =
     let (the_function, _) = StringMap.find fdecl.sfname function_decls in
     let builder = L.builder_at_end context (L.entry_block the_function) in

     let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
     let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
     let char_format_str = L.build_global_stringptr "'%c'\n" "fmt" builder in

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
      let rec onlybind lis = match lis with
          [] -> []
        | SBind(x, y) :: es -> (x, y) :: (onlybind es)
        | _ :: es -> onlybind es
      in let localds = onlybind fdecl.sbody in
      List.fold_left add_local formals localds (*@TODO: ADD LATERfdecl.slocals*)
    in

    (* Return the value for a variable or formal argument. First check
     * locals, then globals *)

    let lookup n lcs = try StringMap.find n lcs
                   with Not_found -> StringMap.find n global_vars
    in
    (*let lookupmem v m = (*let t =*) try StringMap.find v local_vars (*lcs*)
        with Not_found -> StringMap.find v global_vars
        (*@TODO: get pointer to member of Player properly
           in match t with
                A.Player -> if m = "letters" then (ptr to v.letters) else
                  if m = "turn" then (ptr to v.turn) else
                  if m = "guessedWords" then (ptr to v.guessedWords) else
                  if m = "score" then (ptr to v.guessedWords) else
                  raise (Failure "Error: should have been caught at semant")
                | _ -> raise (Failure "Error: should have been caught at semant")*)
    in*)

    let rec inddex (lis: Sast.sexpr list) (i: int) = (*get list elem by index*)
        match lis with elm :: rest ->
                if i = 0 then elm
                else (inddex (rest) (i - 1))
        | _ -> raise (Failure "Index out of bounds")
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder locs ((gtype, e) : sexpr) = match e with
	SLiterali i -> L.const_int i32_t i
      | SLiteralc c -> L.const_int i8_t (int_of_char c)
      | SLiteralb b -> L.const_int i1_t (if b then 1 else 0)
      | SLiterals s -> L.build_global_stringptr s "" builder
      | SNoexpr -> L.const_int i32_t 0
      | Null -> L.const_null (ltype_of_typ gtype)
      | SVariable s -> L.build_load (lookup s locs) s builder
      | SVmember (v, m) ->L.const_int i32_t 0 (*@TODO: change to gep or getter*)
      | SLiterall l ->
         let l' = List.map (expr builder locs) l in
         let len = (List.length l) + 1 in
         let (pty, sentinel) = if gtype = A.Charlist then (i8_t, L.const_int i8_t 0)
             else (string_pointer, L.const_null string_pointer) in
           let y = L.build_array_malloc pty (L.const_int i32_t len) "a1" builder
           in let x = L.build_pointercast y (ltype_of_typ gtype) "a2" builder
           in let addtoar index elem =
             let xx = L.build_gep x [| L.const_int i32_t index |] "a3" builder
             in ignore (L.build_store elem xx builder)
           in let _ = List.iteri addtoar l'
           in let _ = addtoar (len - 1) sentinel
         in x
      | SLiteralm m ->
        let mptrr =
         match m with
           [] -> L.const_null (ltype_of_typ gtype)
         | (ke, vl) :: rest ->
           let type_of_ptr = if gtype = A.Charmap then charmap_t else map_t in
         let mptr = L.undef (type_of_ptr)
         in let mptr' = L.build_malloc (type_of_ptr) "tmp" builder
         in let mptr_actual = L.build_pointercast mptr' (ltype_of_typ gtype) "aptr" builder
         in let addtap mp (k, v) =
           let k' = expr builder locs k
           and v' = expr builder locs v
           in let added_key = L.build_insertvalue mp k' 0 "ak" builder
           in let added_val = L.build_insertvalue added_key v' 1 "av" builder
           in L.build_insertvalue added_val (L.const_null (ltype_of_typ gtype)) 2 "an" builder
         in let x = addtap mptr (ke, vl)
         in let _ = L.build_store x mptr' builder
         in let set_map (k, v) =
           ignore (L.build_call (if gtype = Stringmap then smapset_func else cmapset_func)
             [| mptr_actual; (expr builder locs k); (expr builder locs v)|] "" builder)
         in let _ =  List.iter set_map rest
         in mptr_actual
        in mptrr
      | SAssignm (_, s, e) -> let e' = expr builder locs e in (*@TODO: struct access*)
                          let _  = L.build_store e' (lookup s locs) builder in e'
      | SAssign (s, e) -> let e' = expr builder locs e in
                          let _  = L.build_store e' (lookup s locs) builder in e'
      | SBinop (e1, op, e2) ->
	  let (t, _) = e1 and (tt, _) = e2
	  and e1' = expr builder locs e1
	  and e2' = expr builder locs e2 in
	  if t = A.Int then (match op with
          | A.Add     -> L.build_add
	  | A.Sub     -> L.build_sub
	  | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.Mod     -> L.build_urem
	  | A.Equal   -> L.build_icmp L.Icmp.Eq
	  | A.Neq     -> L.build_icmp L.Icmp.Ne
	  | A.Less    -> L.build_icmp L.Icmp.Slt
	  | A.Leq     -> L.build_icmp L.Icmp.Sle
	  | A.Greater -> L.build_icmp L.Icmp.Sgt
	  | A.Geq     -> L.build_icmp L.Icmp.Sge
          | _ -> raise (Failure "semantic error in codegen")
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
          else raise (Failure "internal error: not binop for string, should not have passed semantic")
          else if t = A.Bool then
            if op = A.And then L.build_and e1' e2' "tmp" builder
  	    else L.build_or e1' e2' "tmp" builder
          else if t = A.Char then (match op with
              A.Equal   -> L.build_icmp L.Icmp.Eq
            | A.Neq     -> L.build_icmp L.Icmp.Ne
            | A.Less    -> L.build_icmp L.Icmp.Slt
            | A.Leq     -> L.build_icmp L.Icmp.Sle
            | A.Greater -> L.build_icmp L.Icmp.Sgt
            | A.Geq     -> L.build_icmp L.Icmp.Sge
            | _ -> raise (Failure "semantic error in codegen")
             ) e1' e2' "tmp" builder
          else raise (Failure "internal error: binop not implemented")
      | SUnop(op, e) ->
	  let _ = e in
          let e' = expr builder locs e in
	  (match op with
            A.Neg                  -> L.build_neg e' "tmp" builder
          | A.Not                  -> L.build_not e' "tmp" builder
          | A.Asc                  -> L.build_call ascii_func [| e' |] "tmp" builder)
      | SNewtobj(t) -> L.const_null (ltype_of_typ t) (*set map to Null*)
      | SNewobj _ ->  (* do we need a match expr first?? *)
         let type_of_ptr = player_t in
         let pptr = L.build_malloc (type_of_ptr) "tmp" builder
         (*in let addtoplayer plyr (s, t, g, l) =  (* score, turn, guessed, letters *)
           let s' = expr builder locs s
           and t' = expr builder locs t
           and g' = expr builder locs g
           and l' = expr builder locs l
         in let added_score = L.build_insertvalue pptr s' 0 "as" builder
         in let added_turn = L.build_insertvalue pptr t' 1 "at" builder
         in let added_guessed = L.build_insertvalue pptr g' 2 "ag" builder
         in let added_letters = L.build_insertvalue pptr l' 3 "al" builder
         in let plyr = addtomap pptr (score, turn, guessed, letters)
         in let set_plyr (s, t, g, l) =
           L.build_call [|(expr builder locs s); (expr builder locs t); (expr builder locs g); (expr builder locs l)|] "" builder
         *)in pptr
      | SNewlis l ->
           if gtype = A.Charlist then  (*1 dimension*)
             let e' = expr builder locs (inddex l 0)
             in let e'' = L.build_add e' (L.const_int i32_t 1) "tmp" builder
             in let y = L.build_array_malloc i8_t e'' "a1" builder
             in let x = L.build_pointercast y string_pointer "a2" builder
             in let addtoar index elem =
               let xx = L.build_gep x [| index |] "a3" builder
               in ignore (L.build_store elem xx builder)
             in let _ = addtoar (e'') (L.const_int i8_t 0) (*sentinel*)
             (*@TODO: for (e') iterations, fill array with 1*)
             in let _ = L.build_call fillis_func [| x; e' |] "" builder
             in x
           else (*2D*)
             let rows = expr builder locs (inddex l 0)
             in let rows' = L.build_add rows (L.const_int i32_t 1) "tmp" builder
             in let cols = expr builder locs (inddex l 1)
             in let y = L.build_array_malloc string_pointer rows' "a1" builder
             in let x = L.build_pointercast y listlist_ptr "a2" builder
             in let addtoar index elem =
               let xx = L.build_gep x [| index |] "a3" builder
               in ignore (L.build_store elem xx builder)
             in let _ = addtoar (rows') (L.const_null string_pointer) (*sentinel*)
             (*@TODO: for (rows) iterations, add the result of snewlis(cols)*)
             in let _ = L.build_call fillislis_func [| x; rows; cols |] "" builder
             in x
      | SCall ("Stringmapset", args) ->
        (*for map setters we need to build a call to assign the result if we
        are setting a variable (or, in the case of stringmap, a vmember)*)
        let (a, b, c) = match args with (aa :: bb :: cc :: []) -> (aa, bb, cc)
          | _ -> raise (Failure("wrong number of arguments")) in
        let llargs = List.rev (List.map (expr builder locs) (List.rev args)) in
        let ans =
          match a with (_, SVariable(var)) -> (*@TODO: SVmember when player is working*)
            let cres = L.build_call smapset_func (Array.of_list llargs) "Smset_result" builder
            in let _  = L.build_store cres (lookup var locs) builder in cres
          | _ -> L.build_call smapset_func (Array.of_list llargs) "Smset_result" builder
        in ans
      | SCall ("Charmapset", args) ->
        let (a, b, c) = match args with (aa :: bb :: cc :: []) -> (aa, bb, cc)
          | _ -> raise (Failure("wrong number of arguments")) in
        let llargs = List.rev (List.map (expr builder locs) (List.rev args)) in
        let ans =
          match a with (_, SVariable(var)) ->
            let cres = L.build_call cmapset_func (Array.of_list llargs) "Cmset_result" builder
            in let _  = L.build_store cres (lookup var locs) builder in cres
          | _ -> L.build_call cmapset_func (Array.of_list llargs) "Cmset_result" builder
        in ans
      | SCall ("Stringmapdestroy", args) -> (*same with destroyers*)
        let (a, b) = match args with (aa :: bb :: []) -> (aa, bb)
          | _ -> raise (Failure("wrong number of arguments")) in
        let llargs = List.rev (List.map (expr builder locs) (List.rev args)) in
        let ans =
          match a with (_, SVariable(var)) -> (*@TODO: SVmember when player is working*)
            let cres = L.build_call smapd_func (Array.of_list llargs) "Smdestroy_result" builder
            in let _  = L.build_store cres (lookup var locs) builder in cres
          | _ -> L.build_call smapd_func (Array.of_list llargs) "Smdestroy_result" builder
        in ans
      | SCall ("Charmapdestroy", args) ->
        let (a, b) = match args with (aa :: bb :: []) -> (aa, bb)
          | _ -> raise (Failure("wrong number of arguments")) in
        let llargs = List.rev (List.map (expr builder locs) (List.rev args)) in
        let ans =
          match a with (_, SVariable(var)) ->
            let cres = L.build_call cmapd_func (Array.of_list llargs) "Cmdestroy_result" builder
            in let _  = L.build_store cres (lookup var locs) builder in cres
          | _ -> L.build_call cmapd_func (Array.of_list llargs) "Cmdestroy_result" builder
        in ans
      | SCall ("readDict", args) -> (*store address of stringmap in dictionary*)
        let llargs = List.rev (List.map (expr builder locs) (List.rev args)) in
        let dict = L.build_call rdict_funct (Array.of_list llargs) "dict_result" builder
        in let _  = L.build_store dict (lookup "dictionary" locs) builder in dict
      | SCall ("Charmapcontains", args) ->
        let llargs = List.rev (List.map (expr builder locs) (List.rev args)) in
        let res = L.build_call cmapc_func (Array.of_list llargs) "cmcontains_result" builder
        in let ans = L.build_icmp L.Icmp.Eq res (L.const_int i8_t 1) "tmp" builder
        in ans
      | SCall ("Stringmapcontains", args) ->
        let llargs = List.rev (List.map (expr builder locs) (List.rev args)) in
        let res = L.build_call smapc_func (Array.of_list llargs) "smcontains_result" builder
        in let ans = L.build_icmp L.Icmp.Eq res (L.const_int i8_t 1) "tmp" builder
        in ans
      | SCall ("listToString", args) ->
        expr builder locs (inddex args 0)
      | SCall ("stringToList", args) ->
        expr builder locs (inddex args 0)
      | SCall (f, args) ->
         let llargs = List.rev (List.map (expr builder locs) (List.rev args))
         in let (fdef, result) =
           match f with
             "InitializeRandom" -> (randi_func, "")
           | "anagram" -> (anagram_func, "anagram_result")
           | "ListlistgetLength" -> (lllen_func, "llen_result")
           | "CharlistgetLength" -> (cllen_func, "clen_result")
           | "StringmapgetLength" -> (smlen_func, "smlen_result")
           | "CharmapgetLength" -> (cmlen_func, "cmlen_result")
           | "Charmapget" -> (cmapget_func, "Charmapget_result")
           | "Stringmapget" -> (smapget_func, "Stringmapget_result")
           | "Charmapgeti" -> (cmapgeti_func, "Charmapgeti_result")
           | "Stringmapgeti" -> (smapgeti_func, "Stringmapgeti_result")
           | "Charlistget" -> (clget_func, "Charlistget_result")
           | "Listlistget" -> (llget_func, "Listlistget_result")
           | "Charlistset" -> (clset_func, "Charlistset_result")
           | "Listlistset" -> (llset_func, "Listlistset_result")
           | "random" -> (rand_funct, "rand_result")
           | "readInput" -> (rinput_funct, "rinput_result")
           (*@TODO: rest of builtins here*)
           | _ ->
             let (ff, fdecl) = try StringMap.find f function_decls
             with Not_found -> raise (Failure ("FAIL " ^ f)) in
             let r = (match fdecl.styp with
                        A.Void -> ""
                      | _ -> f ^ "_result") in (ff, r)
         in
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


      let rec zipper lis x = match lis with
         [] -> []
        | e :: rest -> (e, x) :: (zipper rest x)
     in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    (* Imperative nature of statement processing entails imperative OCaml *)
    let rec stmt builder = function
        (SExpr e, locs) -> let _ = expr builder locs e in builder
      | (SPrint (t, e), locs) ->
          if t = A.String then let _ = L.build_call printf_func [| str_format_str ; (expr builder local_vars (t,e)) |]
	    "printf" builder in builder
         else if t = A.Int then let _ = L.build_call printf_func [| int_format_str ; (expr builder locs (t,e)) |]
           "printf" builder in builder
         else if t = A.Char then let _ = L.build_call printf_func [| char_format_str ; (expr builder locs (t,e)) |]
           "printf" builder in builder
         else if t = A.Bool then
           let thens = [SPrint((A.String, SLiterals("true")))]
           in let elses = [SPrint((A.String, SLiterals("false")))]
           in stmt builder (SIf((t, e), thens, elses), locs)
         else
           let printfun =
             if t = A.Stringmap then printsmap_func
             else if t = A.Charmap then printcmap_func
             else if t = A.Charlist then printil_func
             else if t = A.Listlist then printll_func
             else raise (Failure ("Fail " ^ (A.string_of_typ t)))
           in let _ = L.build_call printfun [| (expr builder locs (t, e)) |]
           "" builder in builder
      | (SIf (predicate, then_stmt, else_stmt), locs) -> (*lifted from microC, comments removed for brevity*)
         let bool_val = expr builder locs predicate in
	 let merge_bb = L.append_block context "merge" the_function in
         let branch_instr = L.build_br merge_bb in

	 let then_bb = L.append_block context "then" the_function in

         let then_builder = List.fold_left stmt (L.builder_at_end context then_bb) (zipper then_stmt locs) in
	 let () = add_terminal then_builder branch_instr in

	 let else_bb = L.append_block context "else" the_function in
         let else_builder = List.fold_left stmt (L.builder_at_end context else_bb) (zipper else_stmt locs) in
	 let () = add_terminal else_builder branch_instr in

	 let _ = L.build_cond_br bool_val then_bb else_bb builder in
	 L.builder_at_end context merge_bb
     | (SWhile (predicate, body), locs) ->  (*lifted from microC, comments removed for brevity*)
         let pred_bb = L.append_block context "while" the_function in
         let _ = L.build_br pred_bb builder in

         let body_bb = L.append_block context "while_body" the_function in
         let while_builder = List.fold_left stmt (L.builder_at_end context body_bb) (zipper body locs) in
         let () = add_terminal while_builder (L.build_br pred_bb) in

         let pred_builder = L.builder_at_end context pred_bb in
         let bool_val = expr pred_builder locs predicate in

         let merge_bb = L.append_block context "merge" the_function in
         let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
         L.builder_at_end context merge_bb
     | (SReturn e, locs) -> let _ = match fdecl.styp with
                              A.Void -> L.build_ret_void builder
                            | _ -> L.build_ret (expr builder locs e) builder
                     in builder
     | (SBind(t, s), locs) -> builder (*already processed*)
     | (SFor (e, body), locs) ->
       (*build temporary counter variable in order to implement for as while*)
       let rec counterbind s =
         try let _ = StringMap.find s local_vars in (counterbind (s ^ s))
         with Not_found -> s
       in let varname = counterbind "_"
       in let add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m
       in let locs = add_local locs (A.Int, varname)
       in let _ = expr builder locs (A.Int, SAssign(varname, (A.Int, SLiterali(0))))
       in let sx' = SBinop((A.Int, SVariable(varname)), A.Less, e)
       in let body' = body @
         [SExpr(A.Int,
                SAssign(varname,
                       (A.Int, SBinop((A.Int, SVariable(varname)),
                                       A.Add,
                                       (A.Int, SLiterali(1))
                                      )
                       ))
               )] in
        let builder = stmt builder ((SWhile((A.Int, sx'), body')), locs) in builder
     | (SForeach(v, (t, s), body), locs) ->
        let type_of_s tr = match tr with A.Listlist -> A.Charlist | A.Charlist -> A.Char
          | A.Charmap -> A.Char | A.Stringmap -> A.String | _ -> raise (Failure "Fatal error: Foreach")
        in let tos = type_of_s t (*the type of v*)
        in let _ = stmt builder ((SBind(tos, v)), locs) (*build space for your temp var*)
        in let rec counterbind s =
          try let _ = StringMap.find s locs in (counterbind (s ^ s))
          with Not_found -> s
        in let varname = counterbind "_"
        in let add_local m (t, n) =
         let local_var = L.build_alloca (ltype_of_typ t) n builder
         in StringMap.add n local_var m
        in let locs = add_local locs (A.Int, varname)
        in let get_fun = if t = A.Listlist || t = A.Charlist
          then (A.string_of_typ t) ^ "get"
          else (A.string_of_typ t) ^ "getnext"
        in let iterer = SExpr(tos, SAssign(v, (tos, SCall(get_fun, [] (*@TODO: ???*))))) (*do a get*)
        in let while_cond = SBinop((A.Int, SVariable(varname)), A.Less,
                                   (A.Int, SCall("Getlength", []))) (*@TODO: fix call, turn to SCallm when that is fixed*)
        in let body' = iterer :: body
        in let _ = stmt builder ((SWhile((A.Int, while_cond), body')), locs)
        in builder
    | (SExit i, _) ->
        let _ = (L.build_call exit_func [| L.const_int i32_t i |]
        "" builder) in builder
    in

    (* Build the code for each statement in the function *)
    let builder = List.fold_left stmt builder (zipper fdecl.sbody local_vars)
     in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Int -> L.build_ret (L.const_int i32_t 0)
      | A.Bool -> L.build_ret (L.const_int i1_t 0)
      | A.Char -> L.build_ret (L.const_int i8_t 0)
      | t -> L.build_ret (L.const_null (ltype_of_typ t)))
  in

  List.iter build_function_body functions;
  the_module
