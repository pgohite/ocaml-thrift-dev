(*
 * C language generator
 *)
open Othrift_exp;;

module CTHRIFT =
struct
  type t = Othrift_exp.document

  let tab = "  "
  let test () =
    Printf.printf "Hello World\n"

  let string_of_fieldtype (f : field_type) : string =
    match f with
    (* member is another user defined data structure *)
    | FT_Ident fname -> (string_of_ident fname)
    (* member is base data type *)
    | FT_BaseType fname -> (string_of_basetype fname)

  (* Generate struct field contents *)
  let string_of_field (f : field) : string =
    (* Ignore thift field-id, field-required *)
    let Field (_, _, ftype, (name, _)) = f in
        (string_of_fieldtype ftype) ^ " " ^ (string_of_ident name)

  (* Generate struct contents, list represent all members of a struct *)
  let generate_fieldlist (lst: field list option) : string =
    let rec string_of_fields l =
      match l with
      | [] -> ""
      | hd :: tl ->  tab ^ (string_of_field hd) ^ ";\n"
                     ^ string_of_fields tl
    in
    match lst with
    | None -> ""
    | Some l -> string_of_fields l

  (* Generate enum contents, list represent all enums in a enum definition *)
  let generate_enumlist (lst : (identifier * int option) list option) : string =
    let rec string_of_enums l =
      match l with
      | [] -> ""
      | (ename, Some value) :: tl ->
           tab ^ (string_of_ident ename) ^ " = " ^ (string_of_int value) ^ ",\n"
           ^ string_of_enums tl
      | (ename, None) :: tl ->
           tab ^ (string_of_ident ename) ^ ",\n"
           ^ string_of_enums tl
    in

    let rec fill_enumlist l (i: int) =
      match l with
      | [] -> []
      | (id, Some value) :: tl ->
          (id, Some value) :: fill_enumlist tl (value + 1)
      | (id, None) :: tl ->
          (id, Some i) :: fill_enumlist tl (i + 1)
    in

    match lst with
    | None -> ""
    | Some l ->
     string_of_enums (fill_enumlist l 0) 

  let string_of_functype ft =
    match ft with
    | FuncType_Void -> "void"
    | FuncType_Field (fld) -> string_of_fieldtype fld
        

  let string_of_args args =
    match args with
    | None -> "void"

  (* Generate one function *)
  let string_of_function (f : functions) : string =
    match f with
    | OneWay_Func (ftype, fname, args, _) ->
        (string_of_functype ftype) ^ " "
        ^ string_of_ident fname ^ " ("
        ^ string_of_args args  ^ ")\n"
        ^ "{\n"
        ^ "    /* One Way TODO */\n"
        ^ "}\n"
    | TwoWay_Func (ftype, fname, args, _) ->
        (string_of_functype ftype) ^ " "
        ^ string_of_ident fname ^ " ("
        ^ string_of_args args  ^ ")\n"
        ^ "{\n"
        ^ "    /* Two Way TODO */\n"
        ^ "}\n"

  (* Generate functions, list represent all functions in a service *)
  let generate_funlist (lst: functions list option) : string =
    let rec string_of_funs l =
      match l with
      | [] -> ""
      | hd :: tl ->  (string_of_function hd) ^ "\n"
                     ^ string_of_funs tl
    in
    match lst with
    | None -> ""
    | Some l -> string_of_funs l

  (* Generate a C typedef code *)
  let generate_typedef (d : typedef) : string = 
    let Typedef (DT_BaseType(bt), id) = d in
      "typedef " ^ (string_of_basetype bt) ^ " " 
      ^ (string_of_ident id)
      ^ ";\n"
 
  (* Generate a C struct code *)
  let generate_struct (d : tstruct) : string =
    let Struct (id, flist) = d  in
      "struct " ^ (string_of_ident id) ^ " {\n"
      ^ (generate_fieldlist flist)
      ^ "};\n"

  (* Generate a C enum code *)
  let generate_enum (d : enum) : string =  
    let Enum (id, elist) = d in 
       "enum " ^ (string_of_ident id) ^ " {\n"
       ^ (generate_enumlist elist)
       ^ "};\n"

  let generate_service (d : service) : string =
    let Service (id, ext, flist) = d  in
      "/* Start Service " ^ (string_of_ident id) ^ "*/\n"
      ^ (generate_funlist flist)
      ^ "/* Start Service " ^ (string_of_ident id) ^ "*/\n"

  (* Generate C  & print definitions *)
  let generate_definitions t =
    let Definitions (lst) = t in
    let rec generate_helper l =
      match l with
      | [] -> ()
      | hd :: tl ->
          let _ =  
          (match hd with
           | DF_Typedef (d) ->
             Printf.printf "%s" ((generate_typedef d) ^ "\n")
           | DF_Struct (d) ->
             Printf.printf "%s" ((generate_struct d) ^ "\n") 
           | DF_Enum (d) ->
             Printf.printf "%s" ((generate_enum d) ^ "\n") 
           | DF_Service (d) ->
             Printf.printf "%s" ((generate_service d) ^ "\n")) in
           generate_helper tl
       in
    generate_helper lst
end;;

module CTHCC = CTHRIFT;; 
