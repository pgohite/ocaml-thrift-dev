(*
   othrift_parse.ml

   Author: Pravin Gohite (pravin.gohite@gmail.com)

   OCAML Parser Grammer for thrift interface definition language
*)

%{
  open Othrift_exp
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> FID
%token <string> STRING

%token TYPE_BOOL
%token TYPE_BYTE
%token TYPE_INT8
%token TYPE_INT16
%token TYPE_INT32
%token TYPE_STRING
%token TYPE_VOID

%token DEF_TYPEDEF
%token DEF_STRUCT
%token DEF_ENUM
%token DEF_FUNCTION
%token DEF_SERVICE
%token DEF_ONEWAY

%token FLD_REQUIRED
%token FLD_OPTIONAL

%token LEFT_CURLY
%token RIGHT_CURLY

%token LEFT_BRACK
%token RIGHT_BRACK

%token LEFT_BRACE
%token RIGHT_BRACE
%token LIST_SEPARATOR

%token COLON
%token COMMA
%token EQUAL 
%token EOF

%start <Othrift_exp.document list option> prog
%%

(* Start parsing from here, if document is empty then return none *)
(* else start parsing contnts*)
prog:
  | EOF       { None }
  | d = parse_document { Some d }
;

(* A thirift document is set of headers such as includes & namespace and*)
(* definitions such as typedef, services enums etc. Parse each of these items*)
(* at a time and construct a ocaml "document" data type as defined in thrift*)
(* expression module*)
parse_document:
  | DEF_TYPEDEF; def_type = parse_def_type; id = parse_identifier
    { [Definitions (DF_Typedef (Typedef (def_type, id)))] }
  | DEF_STRUCT; sname = parse_identifier; LEFT_CURLY;
    field_list = parse_fields;
    RIGHT_CURLY
    { [(Definitions (DF_Struct (Struct (sname, Some (List.rev field_list)))));
		  (Dictionary  (TStruct sname))]} 
  | DEF_ENUM; ename = parse_identifier; LEFT_CURLY;
    enum_list = parse_enums;
    RIGHT_CURLY
    { [(Definitions (DF_Enum (Enum (ename, Some (List.rev enum_list)))));
		   (Dictionary  (TEnum ename))]} 
  | DEF_SERVICE; sname = parse_identifier; LEFT_CURLY;
    funlist = parse_functions; 
    RIGHT_CURLY
    { [Definitions (DF_Service(Service (sname, None, Some funlist)))] }
;

(* Parse thrift function definitions. A thrift funtion can be oneway or two*)
(* Parsing happens left recursively while building a list of parsed function*) 
parse_functions:
  | (* empty *) { [] }
  (* Oneway Funtion *)
  | flist = parse_functions;
		DEF_ONEWAY; ret = parse_fun_return_type; fname = parse_fun_name;
    arg = parse_fun_args; parse_optional(LIST_SEPARATOR) 
    { (OneWay_Func (ret, fname, arg, Throws(None))) :: flist } 
	(* Twoway Funtion *) 
  | flist = parse_functions;
    ret = parse_fun_return_type; fname = parse_fun_name;
    arg = parse_fun_args; parse_optional(LIST_SEPARATOR)
    { (TwoWay_Func (ret, fname, arg, Throws(None))) :: flist } 

parse_fun_name:
  | id  = ID  { Ident id }
  | fid = FID { Ident (String.sub fid 0 ((String.length fid) - 1)) }

parse_fun_return_type:
  | TYPE_VOID { FuncType_Void }
  | ftype = parse_field_type
      { FuncType_Field (ftype) }

parse_fun_args:
  | (* empty *) { None }
  | LEFT_BRACE; RIGHT_BRACE 
	| RIGHT_BRACE
	 { None }
	| LEFT_BRACE; args = parse_fields; RIGHT_BRACE 
	| args = parse_fields; RIGHT_BRACE
    { Some (List.rev args) }

parse_fields:
  | (* empty *) { [] }
  | fld = parse_fields; 
    f_id = INT; COLON;
		field_type = parse_field_type; id = parse_identifier; parse_optional(LIST_SEPARATOR)
      { (Field (Some (FieldID f_id), Required, field_type, (id, None))) :: fld }

parse_enums:
 | (* empty *) { [] }
 | id = parse_enums;
   enum_name = parse_identifier; EQUAL; enum_val = INT; LIST_SEPARATOR
     { (enum_name, Some enum_val) :: id }
 | id = parse_enums;
   enum_name = parse_identifier; LIST_SEPARATOR
   { (enum_name, None) :: id }

parse_def_type:
  | TYPE_BOOL     { DT_BaseType (Bool) }
  | TYPE_BYTE     { DT_BaseType (Byte) }
  | TYPE_INT16    { DT_BaseType (Int16) }
  | TYPE_INT32    { DT_BaseType (Int32) }
  | TYPE_STRING   { DT_BaseType (String) }
;

parse_field_type:
  | TYPE_BOOL     { FT_BaseType (Bool) }
  | TYPE_BYTE     { FT_BaseType (Byte) }
  | TYPE_INT16    { FT_BaseType (Int16) }
  | TYPE_INT32    { FT_BaseType (Int32) }
  | TYPE_STRING   { FT_BaseType (String) }
  | id = ID       { FT_CustomType (Ident id) }
;

parse_identifier:
  | id = ID       { (* let _ = Printf.printf "Parsed ID %s\n" id in *)
                    Ident id }
;

(* Parse optional tokens,*)
(* examples:*)
(*  - end of argument should not have  a comma  *)
(*  - last enum/struct member may not have a comma *)
parse_optional(X):
  | { None }
	| x = X { Some x }