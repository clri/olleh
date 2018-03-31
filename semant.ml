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
  let check_binds (kind : string) (to_check : 'a (*bind*) list) =
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

  (*let globals' = check_binds "global" globals in*)
  let globals' = [] in (*@TODO: FIGURE OUT DYNAMIC DECLARATION*)

  (**** Checking Functions ****)


  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    let add_bind map (name, ty) =
      match ty with Void -> StringMap.add name { typ = Void; fname = name; formals = []; body = [] } map
      | _ -> StringMap.add name {
      typ = Void; fname = name;
      formals = [(ty, "x")]; body = [] } map
    (*@TODO: ADD BUILTINS*)
    in List.fold_left add_bind StringMap.empty [ ("print", Int);
                                                 ("CollectLocalGarbage", Void);
                                                 ("InitializeLocalGarbage", Void)
			                         (*("printb", Bool);
			                         ("printf", Float);
			                         ("printbig", Int)*) ]
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

  let _ = find_func "main" in (* no spoofed programs please *)

  let check_function func =
    (* Make sure no formals are void or duplicates *)
    let formals' = check_binds "formal" func.formals in
    let locals' = [] in

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (globals' @ formals' @ locals' )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s)) in
    let type_of_vmember s m =
      let tvs = try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s)) in
      match tvs, m with (*@TODO: Make more robust*)
        Board, "rows" -> Int
        | _ -> raise (Failure ("Object " ^ s ^ " has no attribute " ^ m))
    in

    (*@TODO: LIST TYPE CHECKING, MAP TYPE CHECKING*)
    (*@TODO: REMOVE AND OTHER FUNCTIONS ON OBJECTS*)

    (*tuple mapper helper function*)
    let rec map_tup f (x, y) =
        (f x, f y)
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Literali  l -> (Int, SLiterali l)
      | Literalc l -> (Char, SLiteralc l)
      | Literals l -> (String, SLiterals l)
      | Literalb l -> (Bool, SLiteralb l)
      | Noexpr     -> (Void, SNoexpr)
      | Null       -> (Void, Null)
      | Variable s       -> (type_of_identifier s, SVariable s)
      | Vmember(s, m) -> (type_of_vmember s m, SVmember(s, m))
      | Literall l -> (List, SLiterall (List.map expr l))
      | Literalm m -> (Map, SLiteralm (List.map (map_tup expr) m))
      | Assign(var, e) as ex ->
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      | Unop(op, e) as ex ->
          let (t, e') = expr e in
          let ty = match op with
            Neg when t = Int -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e ->
          let (t1, e1') = expr e1
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div | Mod when same && t1 = Int   -> Int
          | Add when same && t1 = String -> String
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
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssignm(var, mem, (rt, e'))) (*@TODO: IMPLEMENT BETTER*)
      | Call(fname, args) as call ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e =
            let (et, e') = expr e in
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
      (*@TODO: IMPLEMENT BELOW *)
      | Callm(fname, mname, args) as call ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e =
            let (et, e') = expr e in
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCallm(fname, mname, args'))
      | Newtobj(t1, t2) -> (t1, SNewtobj (t1, t2)) (*@TODO: IMPLEMENT*)
      | Newobj(t1, _) -> (t1, SNewobj (t1, [])) (*@TODO: IMPLEMENT*)

    in

    let check_bool_expr e =
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e')
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | Print(e) -> SPrint( expr e )
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt_list b1, check_stmt_list b2)
      | For(e1, st) -> (*@TODO: Make sure e1 evaluates to an int *)
	  SFor(expr e1, check_stmt_list st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt_list s)
      | Foreach(v, e, sl) -> SForeach(v, expr e, check_stmt_list sl)
      | Exit(i) -> SExit(i)
      | Bind(ty, var) -> SBind(ty, var) (*@TODO: IMPLEMENT*)
      | Assignd(ty, var, e) -> SAssignd(ty, var, expr e) (*@TODO: IMPLEMENT*)
      | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e')
        else raise (
	  Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))

	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)

      and check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []


    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = formals';
      sbody = check_stmt_list func.body (*no err since no block *)
    }
  in (*List.map check_function functions*) (globals', List.map check_function functions)
