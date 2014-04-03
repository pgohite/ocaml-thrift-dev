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

rule read =
  parse
  | white       { read lexbuf }
  | newline     { next_line lexbuf; read lexbuf }
  | int as num  { INT (int_of_string num) }
  | "bool"      { BOOL }
  | "byte"      { BYTE }
  | "i8"        { INT8 }
  | "i16"       { INT16 }
  | "i32"       { INT32 }
  | "typedef"   { TYPEDEF }
  | "struct"    { STRUCT }
  | "required"  { REQUIRED }
  | "optional"  { OPTIONAL }
  | ident as id { ID id }  
  | '"'         { read_string (Buffer.create 17) lexbuf }
  | '{'         { LEFT_BRACE }
  | '}'         { RIGHT_BRACE }
  | '['         { LEFT_BRACK }
  | ']'         { RIGHT_BRACK }
  | ':'         { COLON }
  | ','         { COMMA }
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
