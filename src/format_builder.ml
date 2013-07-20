type token =
  | Token of Token.t
  | Space of int

type t = token list

let initial_buffer_length = 80

let create () = []

let add_token line_builder token =
  (Token token)::line_builder

let add_tokens line_builder tokens =
  let tokens = List.map (fun t -> Token t) tokens in
  (List.rev tokens) @ line_builder

let append left right =
  right @ left

let add_space line_builder n =
  (Space n)::line_builder

let to_string line_builder string_of_token =
  let buffer = Buffer.create initial_buffer_length in
  List.iter
    (function
      | Token t -> Buffer.add_string buffer (string_of_token t)
      | Space n -> Buffer.add_string buffer (String.make n ' '))
    (List.rev line_builder);
  Buffer.contents buffer




