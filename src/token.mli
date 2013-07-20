type t = 
  (* { } ( ) + = =:= : case receive .. etc *)
  | Keyword of string
  | Atom of string
  | Var of string
  (* 
   * Using a string here so we can store actual 
   * representation in code 
   *)
  | Char of string
  (* In this iteration we don't care if a number is valid *)
  | Number of string
  | String of string
  (* A comment has an implicit newline *)
  | Comment of string
  | Newline
  | Dot


val pp_stream : t Stream.t -> unit
val pp_list : t list -> unit
