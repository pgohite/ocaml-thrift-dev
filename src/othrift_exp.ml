(*
   othrift_exp.ml

   Author: Pravin Gohite (pravin.gohite@gmail.com)

   OCAML expressions for thrift interface definition language
*)
 
type identifier =
    | Ident of string

type literal = 
    | Literal of string

type base_type =
    | Bool 
    | Byte 
    | Int16
    | Int32
    | Int64
    | Double
    | String

type constvalue     =
    | IntConst of int
    | DoubleConst of float 
    | ConstList of constvalue list option
    | ConstMap  of (constvalue * constvalue) option

type field_type      =
    | FT_Ident of identifier
    | FT_BaseType of base_type

type definition_type =
    | DT_BaseType of base_type 

type func_type =
    | FuncType_Field of field_type
    | FuncType_Void

type field_id =
    | FieldID of int

type field_req =
    | Required
    | Optional

type field =
    | Field of (field_id option * field_req * field_type *
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
    | Enum of (identifier * (identifier * int option) list option)

type typedef =
    | Typedef of (definition_type * identifier)

type const =
    | Const of (field_type * identifier *  constvalue)

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
;;

let string_of_basetype (bt: base_type) : string =
  match bt with
  | Bool    -> "bool"
  | Int16   -> "short"
  | Int32   -> "int"
  | Int64   -> "long long"
  | Double  -> "float"
  | String  -> "char *"
  | byte    -> "unsigned char"
;;

let string_of_fieldreq req =
  match req with
  | Required -> "required"
  | Optional -> "optional"
;;
