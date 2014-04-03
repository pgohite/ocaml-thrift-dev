(*
   othrift_main.ml

   Author: Pravin Gohite (pravin.gohite@gmail.com)
*)

open Core.Std
open Othrift_exp
open Othrift_lex
open Cthrift_gen
open Lexing


let rec gen_definition (lst: definition list) =
 let _ = CTHCC.generate_definitions (Definitions(lst)) in 
 ()


let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Othrift_parse.prog Othrift_lex.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg; None
  | Othrift_parse.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)


let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
    (match value with
     | Definitions deflist -> let _ = gen_definition deflist in
       parse_and_print lexbuf)
  | None -> ()

let main filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx

      
let _ = (main "../test/test.thrift" ())
