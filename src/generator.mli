(*
 * Generator signature
*)

module type GENERATOR =
  sig
    type t
    val generate_typedef  :  t -> string
    val generate_enum     :  t -> string
    val generate_function :  t -> string
  end
;;
