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

  let globals' = check_binds "global" (List.map bind_to_formalbind (*globals in*)
    (Bind(Stringmap, "dictionary") :: (Bind(Charmap, "letterScores") :: globals))) in

  (**** Checking Functions ****)


  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    let add_bind map (name, ty, argus) =
      StringMap.add name { typ = ty; fname = name; formals = argus; body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("InitializeRandom", Void, []);
                                                 ("anagram", String, [(String, "w")]);
                                                 ("readDict", Void, [(String, "filename")]);
                                                 ("listToString", String, [(Charlist, "lis")]);
                                                 ("Stringmapdestroy", Void, [(Stringmap, "k"); (String, "s")]);
                                                 ("Stringmapcontains", Bool, [(Stringmap, "k"); (String, "s")]);
                                                 ("StringmapgetLength", Int, [(Stringmap, "k")]);
                                                 ("Charmapdestroy", Void, [(Charmap, "k"); (Char, "s")]);
                                                 ("Charmapcontains", Bool, [(Charmap, "k"); (Char, "s")]);
                                                 ("CharmapgetLength", Int, [(Stringmap, "k")]);
                                                 ("ListlistgetLength", Int, [(Listlist, "k")]);
                                                 ("CharlistgetLength", Int, [(Charlist, "k")]);
                                                 ("Listlistget", Charlist, [(Listlist, "k"); (Int, "s")]);
                                                 ("Charlistget", Char, [(Charlist, "k"); (Int, "s")]);
                                                 ("Listlistset", Void, [(Listlist, "k"); (Int, "s"); (Char, "c")]);
                                                 ("Charlistset", Void, [(Charlist, "k"); (Int, "s"); (Char, "c")]);
                                                 ("stringToList", Charlist, [(String, "lis")]);
                                                 ("readInput", String, []);
                                                 ("Charmapset", Void, [(Charmap, "k"); (Char, "c"); (Int, "s")]);
                                                 ("Stringmapset", Void, [(Stringmap, "k"); (String, "c"); (Int, "s")]);
                                                 ("Stringmapget", Int, [(Stringmap, "k"); (String, "c")]);
                                                 ("Charmapget", Int, [(Stringmap, "k"); (Char, "c")])
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
       if lvaluet = rvaluet then lvaluet
       else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbolz = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (locals' )
    in
    let add_local_symbol (ty, name) symbols =
        let x = try StringMap.find name symbols with Not_found -> Void
        in if x = Void then StringMap.add name ty symbols
        else raise (Failure "Duplicate variable")
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s symbols =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s)) in
    let type_of_vmember tvs m  =
      match tvs, m with
        | Player, "Score" -> Int
        | Player, "turn" -> Bool
        | Player, "guessedWords" -> Stringmap
        | Player, "letters" -> Charlist
        | _ -> raise (Failure ("Object of type " ^ (string_of_typ tvs) ^ " has no attribute " ^ m))
    in


    (*tuple mapper helper function*)
    let rec map_tup f s (x, y) =
        (f s x, f s y)
    in
    let rec map_tup_nos f (x, y) =
        (f x, f y)
    in
    let rec map_over_two f syms lis =
        match lis with
          [] -> []
        | a :: b -> (f syms a) :: (map_over_two f syms b)
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr symbols exx =
      match exx with
        Literali l -> ((Int, SLiterali l), symbols)
      | Literalc l -> ((Char, SLiteralc l), symbols)
      | Literals l -> ((String, SLiterals l), symbols)
      | Literalb l -> ((Bool, SLiteralb l), symbols)
      | Noexpr     -> ((Void, SNoexpr), symbols)
      | Null       -> ((Void, Null), symbols)
      | Variable s  -> ((type_of_identifier s symbols, SVariable s), symbols)
      | Vmember(e, m) ->
        let ((t', e'), symbols') = expr symbols e in
        let tov = type_of_vmember t' m in
        ((tov, SVmember((t', e'), m)), symbols')
      | Literall l ->
          let l' = map_over_two expr symbols l in
          let is_char b ((t, _), _) = b && (t = Char) in
          let lchar = List.fold_left is_char true l' in
          let is_list b ((t, _), _) = b && (t = Charlist) in
          let llist = List.fold_left is_list true l' in
          let l'' = List.map fst l' in
            if lchar then ((Charlist, SLiterall(l'')), symbols) else
              if llist then ((Listlist, SLiterall(l'')), symbols) else
                raise (Failure ("List of improper type"))
      | Literalm m ->
          let m' = List.map (map_tup expr symbols) m in
          let m'' = List.map (map_tup_nos fst) m' in
          let is_charkey b ((t, _), _) = b && (t = Char) in
          let mchar = List.fold_left is_charkey true m'' in
          let is_stringkey b ((t, _), _) = b && (t = String) in
          let mstrng = List.fold_left is_stringkey true m'' in
          let is_intval b (_, (t, _)) = b && (t = Int) in
          let mint = List.fold_left is_intval true m'' in
          if mint then
            if (mchar && not(mstrng)) then ((Charmap, SLiteralm(m'')), symbols) else
              if (mstrng && not(mchar)) then ((Stringmap, SLiteralm(m'')), symbols) else
                raise (Failure ("Map type cannot be inferred"))
          else raise (Failure ("Map value must be integer"))
      | Assign(var, e) as ex ->
          let lt = type_of_identifier var symbols
          and ((rt, e'), symbols') = expr symbols e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in let fty = check_assign lt rt err
          in let symbols'' = if fty != lt then StringMap.add var fty symbols'
            else symbols'
          in ((fty, SAssign(var, (rt, e'))), symbols'')
      | Unop(op, e) as ex ->
          let ((t, e'), symbols') = expr symbols e in
          let ty = match op with
            Neg when t = Int -> t
          | Not when t = Bool -> Bool
          | Asc when t = Int -> Char
          | _ -> raise (Failure ("illegal unary operator " ^
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in ((ty, SUnop(op, (t, e'))), symbols)
      | Binop(e1, op, e2) as e ->
          let ((t1, e1'), symbols') = expr symbols e1
          in let ((t2, e2'), symbols'') = expr symbols' e2 in
          let t1' = t1 in let t1 = if e1 = Null then t2 else t1' in
          let t2' = t2 in let t2 = if e2 = Null then t1 else t2' in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div | Mod when same && t1 = Int   -> Int
          | Add when same && t1 = String -> String
          | Add when t1 = String && t2 = Int -> String
          | Equal | Neq when same && (t1 = Int || t1 = Char || t1 = Bool)  -> Bool
          | Equal when ((e1 = Null && (t2 = Listlist || t2 = Stringmap))
            || (e2 = Null && (t1 = Listlist || t1 = Stringmap))) -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Char) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in ((ty, SBinop((t1, e1'), op, (t2, e2'))), symbols'')
      | Assignm(var, mem, e) as ex ->
          let ((lt, var'), sy') = expr symbols var in
          let ((rt, e'), symbols') = expr sy' e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in ((check_assign lt rt err, SAssignm((lt, var'), mem, (rt, e'))), symbols')
      | Call(fname, args) as call ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e =
            let ((et, e'), _) = expr symbols e in
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in ((fd.typ, SCall(fname, args')), symbols)
      | Callm(vname, fname, args) as call ->
          let ((ty, ee), sy') = expr symbols vname in
          let fd = find_func ((string_of_typ ty) ^ fname) in
          let param_length = List.length fd.formals in
          if List.length args != param_length - 1 then
            raise (Failure ("expecting " ^ string_of_int (param_length - 1) ^
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e =
            let ((et, e'), _) = expr symbols e in
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals (vname :: args)
          in ((fd.typ, SCall((string_of_typ ty) ^ fname, args')), symbols)
      | Newtobj(t1) ->
          if (t1 = Stringmap) then ((t1, SNewtobj(t1)), symbols)
          else if (t1 = Charmap) then ((t1, SNewtobj(t1)), symbols)
          else raise (Failure ("Object cannot be initialized with type"))
      | Newlis(t1, e2) ->
          if (t1 = Charlist && List.length e2 != 1) || (t1 = Listlist && List.length e2 != 2) then
            raise (Failure "Unexpected fatal compiler error") (*parser should have caught this*)
          else
              let l' = map_over_two expr symbols e2 in
              let is_int b ((t, _), _) = b && (t = Int) in
              let lint = List.fold_left is_int true l' in
              if not lint then raise (Failure ("Cannot create new list of non-integer args"))
              else ((t1, SNewlis(List.map fst l')), symbols)
      | Newobj(t1, argus)  ->
          match t1 with
              Player ->
                let rec pla ars = match ars with
                    [] -> []
                  | (v, ex) :: ars' ->
                      let sv va = match va with Variable(x) -> SVariable(x)
                       | _ -> raise (Failure "compiler error") in
                      let ((t, ex'), _) = expr symbols ex in
                        if ((v = Variable("score") && t = Int) ||
                            (v = Variable("turn") && t = Bool) ||
                            (v = Variable("guessedWords") && t = Stringmap) ||
                            (v = Variable("letters") && t = Charlist))
                        then ((t, sv v), (t, ex')) :: pla ars'
                        else raise (Failure "Illegal argument to Player")
                in
                let sargus = pla argus in
                  ((t1, SNewobj(sargus)), symbols)
              | _ -> raise (Failure ("Object needs type initialization"))

    in

    let check_bool_expr symbols e =
      let ((t', e'), symbols') = expr symbols e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else ((t', e'), symbols')
    in
    let check_int_expr symbols e =
      let ((t', e'), symbols') = expr symbols e
      and err = "expected Int expression in " ^ string_of_expr e
      in if t' != Int then raise (Failure err) else ((t', e'), symbols')
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt symbols sstm = match sstm with
        Expr e -> let (e', symbols') = expr symbols e in (SExpr (e'), symbols')
      | Print(e) ->
          let ((t, se), symbols')  = expr symbols e in
            if (t = Player || t = Void) then
              raise (Failure ("Cannot print expr of type " ^ string_of_typ t))
            else
              (SPrint((t, se)), symbols')
      | If(p, b1, b2) -> (*keep track of any new variables introduced for conflicts*)
          let (sifs, stmo) = check_bool_expr symbols p in
          let (sths, stm') = check_stmt_list stmo (List.rev b1) in
          let (sels, stm'') = check_stmt_list stm' (List.rev b2)
          in (SIf(sifs, sths, sels), stm'')
      | For(e1, st) -> (*Make sure e1 evaluates to an int *)
          let (sfo, stmo) = check_int_expr symbols e1 in
          let (sbo, stm') = check_stmt_list stmo (List.rev st) in
	  (SFor(sfo, sbo), stm')
      | While(p, s) ->
          let (sifs, stmo) = check_bool_expr symbols p in
          let (sths, stm') = check_stmt_list stmo (List.rev s) in
          (SWhile(sifs, sths), stm')
      | Foreach(v, e, sl) ->
          let ((t, e'), symbols') = expr symbols e in
          if t != Charlist && t!= Listlist && t != Stringmap && t != Charmap then
            raise (Failure "Can't foreach if it's not a list or map")
            else
          let ty = if t = Listlist then Charlist else if t = Stringmap then String else Char
          in let syms = add_local_symbol (ty, v) symbols' in
          let (sths, stm') = check_stmt_list syms (List.rev sl) in
          (SForeach(v, (t, e'), sths), stm')
      | Exit(i) -> (SExit(i), symbols)
      | Bind(ty, var) ->
        (*check if variable exists *)
        (SBind(ty, var), add_local_symbol (ty, var) symbols)
      | Return e -> let ((t, e'), symbols') = expr symbols e in
        if t = func.typ then (SReturn (t, e'), symbols')
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
  in  (globals', List.map check_function functions)
