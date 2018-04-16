(* Semantic checking for the olleh compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each statement, then check each function *)

let check (*functions*) (globals, functions) =

  (* Check if a certain kind of binding has void type or is a duplicate
     of another, previously checked binding. If the stmt is NOT a binding
     then we ignore it. If it matches SBind(), SAssignd() then it is a
     bind and we need to do stuff *)
  let check_binds (kind : string) (to_check : (*'a*) formalbind list) =
    let check_it checked binding =
      let void_err = "illegal void " ^ kind ^ " " ^ snd binding
      and dup_err = "duplicate " ^ kind ^ " " ^ snd binding
      in match binding with
        (* No void bindings *)
        (Void, _) -> raise (Failure void_err)
      | (_, n1) -> match checked with
                    (* No duplicate bindings *)
                      ((_, n2) :: _) when n1 = n2 -> raise (Failure dup_err)
                    | _ -> binding :: checked
    in let _ = List.fold_left check_it [] (List.sort compare to_check)
       in to_check
  in

  (**** Checking Global Variables ****)
  let bind_to_formalbind = function
      Bind(t, x) -> (t, x)
      | _ -> raise (Failure "Error: Illegal stmt treated as bind")
  in

  let globals' = check_binds "global" (List.map bind_to_formalbind globals) in

  (**** Checking Functions ****)


  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    let add_bind map (name, ty, argus) =
      StringMap.add name { typ = ty; fname = name; formals = argus; body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("CollectLocalGarbage", Void, []);
                                                 ("InitializeLocalGarbage", Void, []);
                                                 ("InitializeRandom", Void, []);
                                                 ("scramble", String, [(String, "w")]);
                                                 ("reverse", String, [(String, "w")]);
                                                 ("anagram", String, [(String, "w")]);
                                                 ("readDict", String, [(String, "filename")]);
                                                 ("listToString", String, [(List, "lis")]);
                                                 ("subStrings", Map, [(String, "w")]);
                                                 ("random", Int, [(Int, "x")])
                                                 (*@TODO: add builtins for objects, of the form "MapgetLength" etc*)
                                                  ]
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err
       | _ ->  StringMap.add n fd map
  in

  (* Collect all other function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  (*let global_context = find_func "main" in (* no spoofed programs please *)
  let check_main =
        (*no formals. check the usage of locals*)
        let in_context = [] in

        ()
  in*)

  let check_function func =
    (* Make sure no formals are void or duplicates *)
    let formals' = check_binds "formal" (func.formals @ globals') in
    let locals' = formals' (*(check_binds "formal" (func.formals)) @ gic*) in

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbolz = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (locals' )
    in
    let add_local_symbol (ty, name) symbols = StringMap.add name ty symbols
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s symbols =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s)) in
    let type_of_vmember s m symbols =
      let tvs = try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s)) in
      match tvs, m with
          Board, "rows" -> Int
        | Board, "cols" -> Int
        | Board, "letters" -> List
        | Player, "Score" -> Int
        | Player, "turn" -> Bool
        | Player, "guessedWords" -> Map
        | Player, "letters" -> List
        | _ -> raise (Failure ("Object " ^ s ^ " of type " ^ (string_of_typ tvs) ^ " has no attribute " ^ m))
    in


    (*tuple mapper helper function*)
    let rec map_tup f s (x, y) =
        (f s x, f s y)
    in
    let rec map_over_two f syms lis =
        match lis with
          [] -> []
        | a :: b -> (f syms a) :: (map_over_two f syms b)
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr symbols exx =
      match exx with
        Literali l -> (Int, SLiterali l)
      | Literalc l -> (Char, SLiteralc l)
      | Literals l -> (String, SLiterals l)
      | Literalb l -> (Bool, SLiteralb l)
      | Noexpr     -> (Void, SNoexpr)
      | Null       -> (Void, Null)
      | Variable s       -> (type_of_identifier s symbols, SVariable s)
      | Vmember(s, m) -> (type_of_vmember s m symbols, SVmember(s, m))
      | Literall l ->
          let l' = map_over_two expr symbols l in
          let is_int b (t, _) = b && (t = Int) in
          let lint = List.fold_left is_int true l' in
          let is_char b (t, _) = b && (t = Char) in
          let lchar = List.fold_left is_char true l' in
          let is_list b (t, _) = b && (t = List) in
            (*@TODO: enforce 2d list of char only*)
          let llist = List.fold_left is_list true l' in
          let lempty = List.length l = 0 in
          if lempty then (List, SLiterall(Void, l')) else
            if lint then (List, SLiterall(Int, l')) else
              if lchar then (List, SLiterall(Char, l')) else
                if llist then (List, SLiterall(List, l')) else
                raise (Failure ("List of improper type"))
      | Literalm m ->
          (*@TODO: implement*)
           (Map, SLiteralm(Void, []))
      | Assign(var, e) as ex ->
          let lt = type_of_identifier var symbols
          and (rt, e') = expr symbols e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      | Unop(op, e) as ex ->
          let (t, e') = expr symbols e in
          let ty = match op with
            Neg when t = Int -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e ->
          let (t1, e1') = expr symbols e1
          and (t2, e2') = expr symbols e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div | Mod when same && t1 = Int   -> Int
          | Add when same && t1 = String -> String
          | Add when t1 = String && t2 = Int -> String
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Assignm(var, mem, e) as ex ->
          let lt = type_of_vmember var mem symbols
          and (rt, e') = expr symbols e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssignm(var, mem, (rt, e')))
      | Call(fname, args) as call ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e =
            let (et, e') = expr symbols e in
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
      | Callm(vname, fname, args) as call ->
          let ty = type_of_identifier vname symbols in
          let fd = find_func ((string_of_typ ty) ^ fname) in
          let param_length = List.length fd.formals in
          if List.length args != param_length - 1 then
            raise (Failure ("expecting " ^ string_of_int (param_length - 1) ^
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e =
            let (et, e') = expr symbols e in
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals (Variable(vname) :: args)
          in (fd.typ, SCall((string_of_typ ty) ^ fname, args'))
      | Newtobj(t1, t2) ->
          if t1 = Map && (t2 = String || t2 = Char) then (*@TODO: add list?*)
                 (t1, SNewtobj(t2))
          else raise (Failure ("Object cannot be initialized with type"))
      | Newobj(t1, argus)  ->
          match t1 with (*@TODO: check these types*)
              Player ->
                let rec pla ars = match ars with
                    [] -> []
                  | (v, ex) :: ars' ->
                      let sv va = match va with Variable(x) -> SVariable(x)
                       | _ -> raise (Failure "compiler error") in
                      let (t, ex') = expr symbols ex in
                        if ((v = Variable("score") && t = Int) ||
                            (v = Variable("turn") && t = Bool) ||
                            (v = Variable("guessedWords") && t = Map (*&&
                              (fst ex' = String || fst ex' = Void)*)) ||
                            (v = Variable("letters") && t = List (*&&
                              (fst ex' = Char ||
                                (fst ex' = Void))*)))
                        then ((t, sv v), (t, ex')) :: pla ars'
                        else raise (Failure "Illegal argument to Player")
                  | _ -> raise (Failure "Illegal argument to Player")
                in
                let sargus = pla argus in
                  (t1, SNewobj(sargus))
              | Board ->
                (t1, SNewobj([])) (*@TODO: IMPLEMENT*)
              | _ -> raise (Failure ("Object needs type initialization"))

    in

    let check_bool_expr symbols e =
      let (t', e') = expr symbols e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e')
    in
    let check_int_expr symbols e =
      let (t', e') = expr symbols e
      and err = "expected Int expression in " ^ string_of_expr e
      in if t' != Int then raise (Failure err) else (t', e')
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt symbols sstm = match sstm with
        Expr e -> (SExpr (expr symbols e), symbols)
      | Print(e) -> (SPrint( expr symbols e ), symbols) (*@TODO: type checking*)
      | If(p, b1, b2) -> (*keep track of any new variables introduced for conflicts*)
          let sifs = check_bool_expr symbols p in
          let (sths, stm') = check_stmt_list symbols b1 in
          let (sels, stm'') = check_stmt_list stm' b2
          in (SIf(sifs, sths, sels), stm'')
      | For(e1, st) -> (*@TODO: Make sure e1 evaluates to an int *)
          let sfo = check_int_expr symbols e1 in
          let (sbo, stm') = check_stmt_list symbols st in
	  (SFor(sfo, sbo), stm')
      | While(p, s) ->
          let sifs = check_bool_expr symbols p in
          let (sths, stm') = check_stmt_list symbols s in
          (SWhile(sifs, sths), stm')
      | Foreach(v, e, sl) ->
          let (t, e') = expr symbols e in
          if t != List && t != Map then raise (Failure "Can't foreach if it's not a list or map")
            else
          let syms = add_local_symbol (t, v) symbols in
          let (sths, stm') = check_stmt_list syms sl in
          (SForeach(v, (t, e'), sths), stm')
      | Exit(i) -> (SExit(i), symbols)
      | Bind(ty, var) -> (SBind(ty, var), add_local_symbol (ty, var) symbols)
      | Return e -> let (t, e') = expr symbols e in
        if t = func.typ then (SReturn (t, e'), symbols)
        else raise (
	  Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))


      and check_stmt_list symbols stms =
          match stms with
              [Return _ as s] ->  ([fst (check_stmt symbols s)], symbols)
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | s :: ss         -> let (sast, symbols') = check_stmt symbols s
                                 in let (slis, symbols'') = check_stmt_list symbols' ss
                                 in (sast :: slis, symbols'')
            | []              -> ([], symbols)


    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = formals';
      sbody = fst (check_stmt_list (if func.fname = "main" then StringMap.empty else symbolz) func.body) (*no err since no block *)
    }
  in (*List.map check_function functions*) (globals', List.map check_function functions )
