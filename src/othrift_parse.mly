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

%token COLON
%token COMMA
%token EQUAL 
%token EOF

%start <Othrift_exp.document option> prog
%%

(* Start parsing from here *)
prog:
  | EOF       { None }
  | d = parse_document { Some d }
;

parse_document:
  | DEF_TYPEDEF; def_type = parse_def_type; id = parse_identifier
    { Definitions [(DF_Typedef (Typedef (def_type, id)))] }

  | DEF_STRUCT; sname = parse_identifier; LEFT_CURLY;
    field_list = parse_fields;
    RIGHT_CURLY
    { Definitions [(DF_Struct (Struct (sname, Some (List.rev field_list))))] } 

  | DEF_ENUM; ename = parse_identifier; LEFT_CURLY;
    enum_list = parse_enums;
    RIGHT_CURLY
    { Definitions [(DF_Enum (Enum (ename, Some (List.rev enum_list))))] } 

  (* ONEWAY VOID Funtion *) 
  | DEF_SERVICE; sname = parse_identifier; LEFT_CURLY;
    funlist = parse_functions; 
    RIGHT_CURLY
    { Definitions [(DF_Service(Service (sname, None, Some funlist)))] }
;

parse_functions:
  | (* empty *) { [] }
  | flist = parse_functions;
    DEF_ONEWAY; ret = parse_fun_return_type; fname = parse_fname;
    arg = parse_fun_args 
   { (OneWay_Func (ret, fname, arg, Throws(None))) :: flist } 

  | flist = parse_functions;
    ret = parse_fun_return_type; fname = parse_fname;
    arg = parse_fun_args 
   { (TwoWay_Func (ret, fname, arg, Throws(None))) :: flist } 

parse_fname:
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

parse_fields:
  | (* empty *) { [] }
  | fld = parse_fields; 
    f_id = INT; COLON; field_type = parse_field_type; id = parse_identifier
    { (Field (Some (FieldID f_id), Required, field_type, (id, None))) :: fld }

parse_enums:
 | (* empty *) { [] }
 | id = parse_enums;
   enum_name = parse_identifier; EQUAL; enum_val = INT; COMMA
   { (enum_name, Some enum_val) :: id }
 | id = parse_enums;
   enum_name = parse_identifier; COMMA
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
  | id = ID       { (* let _ = Printf.printf "Parsed FieldType %s\n" id in *)
                    FT_Ident (Ident id) }
;

parse_identifier:
  | id = ID       { (* let _ = Printf.printf "Parsed ID %s\n" id in *)
                    Ident id }
;
