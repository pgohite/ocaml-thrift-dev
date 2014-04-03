(*
   othrift_exp.ml

   Author: Pravin Gohite (pravin.gohite@gmail.com)

   OCAML expressions for thrift interface definition language
*)
 
type identifier =
    | Ident of string
type literal = 
    | Literal of string

type basetype =
    | Bool 
    | Byte 
    | Int16
    | Int32
    | Int64
    | Double
    | String
    | Binary

type constvalue     =
    | IntConst of int
    | DoubleConst of float 
    | ConstList of constvalue list option
    | ConstMap  of (constvalue * constvalue) option

type fieldtype      =
    | FT_Ident of identifier
    | FT_BaseType of basetype

type definition_type =
    | DT_BaseType of basetype 

type func_type =
    | FuncType_Field of fieldtype
    | FuncType_void

type field_id =
    | FieldID of int

type field_req =
    | Required
    | Optional

type field =
    | Field of (field_id option * field_req * fieldtype *
                (identifier * constvalue option)) 
type throws =
    | Throws of field list option

type functions =
    | OneWay_Func of (func_type * identifier * field list option * throws)
    | TwoWay_Func of (func_type * identifier * field list option * throws)

type service =
    | Service of (identifier * identifier option * functions list option)

type exceptions =
    | Exception of (identifier * field list)

type tstruct =
    | Struct of (identifier * field list option)

type enum =
    | Enum of (identifier * (identifier * int) list)

type typedef =
    | Typedef of (definition_type * identifier)

type const =
    | Const of (fieldtype * identifier *  constvalue)

type definition =
    | DF_Const of const
    | DF_Typedef of typedef
    | DF_Enum of enum
    | DF_Struct of tstruct
    | DF_Service of service
    | DF_Exception of exceptions

type document =
    | Definitions of (definition list)

(* String Conversion Functions *)

let string_of_ident (id: identifier) : string =
  let (Ident str) = id in str

let string_of_basetype (bt: basetype) : string =
  match bt with
  | Bool    -> "bool"
  | Int16   -> "short"
  | Int32   -> "int"
  | Int64   -> "long long"
  | Double  -> "float"
  | String  -> "char *"
  | Binary  -> "char *"
;;
let string_of_fieldreq req =
  match req with
  | Required -> "required"
  | Optional -> "optional"

let string_of_field (f : field) : string =
 let Field (_, _, ftype, (id, _)) = f in
  match ftype with
  | FT_Ident id -> (string_of_ident id) ^ " " ^ (string_of_ident id)
  | FT_BaseType bt -> (string_of_basetype bt) ^ " " ^ (string_of_ident id)


let string_of_fieldlist (lst: field list option) : string =
  let rec string_of_fields l =
    match l with
    | [] -> ""
    | hd :: tl ->  (string_of_field hd) ^ "\n" ^ string_of_fields tl
  in

  match lst with
  | None -> ""
  | Some l -> string_of_fields l
;;


