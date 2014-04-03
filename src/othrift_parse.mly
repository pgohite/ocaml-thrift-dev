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
%token <string> STRING
%token BOOL
%token BYTE
%token INT8
%token INT16
%token INT32
%token TYPEDEF
%token STRUCT
%token REQUIRED
%token OPTIONAL
%token ENUM
%token FUNCTION
%token SERVICE
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token COLON
%token COMMA
%token EOF

%start <Othrift_exp.document option> prog
%%

prog:
  | EOF       { None }
  | d = parse_document { Some d }
;

parse_document:
  | TYPEDEF; def_type = parse_def_type; id = parse_identifier
    { Definitions [(DF_Typedef (Typedef (def_type, id)))] }

  | STRUCT; sname = parse_identifier LEFT_BRACE; field_list = parse_fields; RIGHT_BRACE
    { Definitions [(DF_Struct (Struct (sname, Some (List.rev field_list))))] } 
;
  
parse_fields:
  | (* empty *) { [] }
  | fld = parse_fields; 
    f_id = INT; COLON; field_type = parse_field_type; id = parse_identifier
    { (Field (Some (FieldID f_id), Required, field_type, (id, None))) :: fld }

parse_def_type:
  | BOOL     { DT_BaseType (Bool) }
  | BYTE     { DT_BaseType (Byte) }
  | INT16    { DT_BaseType (Int16) }
  | INT32    { DT_BaseType (Int32) }
;

parse_field_type:
  | BOOL     { FT_BaseType (Bool) }
  | BYTE     { FT_BaseType (Binary) }
  | INT16    { FT_BaseType (Int16) }
  | INT32    { FT_BaseType (Int32) }
;

parse_identifier:
  | id = ID  { Ident id }
;
