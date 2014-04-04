(*
   othrift_exp.ml

   Author: Pravin Gohite (pravin.gohite@gmail.com)

   OCAML lexer for thrift interface definition language
*)

{
open Lexing
open Othrift_parse
open Othrift_exp

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}


let int = '-'? ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let func_ident = ident ['(']
let separator  = [';' ',']

rule read =
  parse
  | white       { read lexbuf }
  | newline     { next_line lexbuf; read lexbuf }
  | int as num  { INT (int_of_string num) }
  | "bool"      { TYPE_BOOL }
  | "byte"      { TYPE_BYTE }
  | "i8"        { TYPE_INT8 }
  | "i16"       { TYPE_INT16 }
  | "i32"       { TYPE_INT32 }
  | "string"    { TYPE_STRING }
  | "void"      { TYPE_VOID }

  | "typedef"   { DEF_TYPEDEF }
  | "struct"    { DEF_STRUCT }
  | "enum"      { DEF_ENUM }
  | "service"   { DEF_SERVICE }

  | "required"  { FLD_REQUIRED }
  | "optional"  { FLD_OPTIONAL }
  | "oneway"    { DEF_ONEWAY }
  
  | ident as id      { ID id }  
  | func_ident as id { FID id }  

  | '"'         { read_string (Buffer.create 17) lexbuf }
  | '{'         { LEFT_CURLY }
  | '}'         { RIGHT_CURLY }
  | '('         { LEFT_BRACE }
  | ')'         { RIGHT_BRACE }
  | '['         { LEFT_BRACK }
  | ']'         { RIGHT_BRACK }
  | ':'         { COLON }
	| separator   { LIST_SEPARATOR }
  | '='         { EQUAL }

  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) } 
