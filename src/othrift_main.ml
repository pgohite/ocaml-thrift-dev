(*
   othrift_main.ml

   Author: Pravin Gohite (pravin.gohite@gmail.com)
*)

open Core.Std
open Othrift_exp
open Othrift_lex
open Cthrift_gen
open Lexing


let rec gen_definition (doclist: document list) =
		CTHCC.generate_definitions doclist

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


let rec parse_and_print doclist lexbuf =
  match parse_with_error lexbuf with
  | Some value -> parse_and_print (doclist @ value) lexbuf
  | None -> doclist

let main filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
	(* Parse thrift file *)
  let doc_parsed = parse_and_print [] lexbuf in
	
	(* Resolve symbols *)
	(* let doc_resolved = resolve_symbols doc_parsed in *)
	
	(*Generate definitions *)
	let _ = gen_definition doc_parsed in
	
  In_channel.close inx

      
let _ = (main "../test/test.thrift" ())
