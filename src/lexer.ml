(*pp camlp4o *)

let initial_buffer_size = 10


let keyword_or_atom string =
  match string with
    | "case"
    | "of"
    | "receive"
    | "if"
    | "try"
    | "catch"
    | "with"
    | "after"
    | "when"
    | "begin"
    | "end"
    | "fun" ->
      Token.Keyword string
    | _ ->
      Token.Atom string


let rec lex = parser
  (* Ignore whitespace except new line *)
  | [< _ = whitespace; stream >] -> lex stream
  | [< ''\n'; stream >] -> [< 'Token.Newline; lex stream >]
  | [< ''.'; stream >] -> [< 'Token.Dot; lex stream >]
  | [< ''%'; stream >] ->
    let buffer = Buffer.create initial_buffer_size in
    lex_comment buffer stream
  | [< ' ('\''); stream >] ->
    let buffer = Buffer.create initial_buffer_size in
    Buffer.add_char buffer '\'';
    lex_atom_quoted buffer stream
  | [< ' ('a' .. 'z') as first_char; stream >] ->
    let buffer = Buffer.create initial_buffer_size in
    Buffer.add_char buffer first_char;
    lex_atom buffer stream
  | [< ' ('A' .. 'Z' | '_') as first_char; stream >] ->
    let buffer = Buffer.create initial_buffer_size in
    Buffer.add_char buffer first_char;
    lex_var buffer stream
  | [< ' ('0' .. '9') as first_digit; stream >] ->
    let buffer = Buffer.create initial_buffer_size in
    Buffer.add_char buffer first_digit;
    lex_number buffer stream
  | [< ''$'; stream >] ->
    lex_char stream
  | [< ' ('"'); stream >] ->
    let buffer = Buffer.create initial_buffer_size in
    lex_string buffer stream
  | [< ''='; stream >] ->
    lex_equal stream
  | [< ''-'; stream >] ->
    lex_minus stream
  | [< 'c; stream >] ->
    [< 'Token.Keyword (String.make 1 c); lex stream >]
  | [< >] -> [< >]
and lex_comment buffer = parser
  | [< ''\n'; stream >] ->
    [< 'Token.Comment (Buffer.contents buffer); 'Token.Newline; lex stream >]
  | [< 'c; stream >] ->
    Buffer.add_char buffer c;
    lex_comment buffer stream
  | [< >] -> [< 'Token.Comment (Buffer.contents buffer) >]
and lex_atom_quoted buffer = parser
  | [< ' ('\''); stream >] ->
    Buffer.add_char buffer '\'';
    [< 'Token.Atom (Buffer.contents buffer); lex stream >]
  | [< 'c; stream >] ->
    Buffer.add_char buffer c;
    lex_atom_quoted buffer stream
and lex_atom buffer = parser
  | [< ' ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_') as c; stream >] ->
    Buffer.add_char buffer c;
    lex_atom buffer stream
  | [< stream >] ->
    [< 'keyword_or_atom (Buffer.contents buffer); lex stream >]
and lex_var buffer = parser
  | [< ' ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_') as c; stream >] ->
    Buffer.add_char buffer c;
    lex_var buffer stream
  | [< stream >] ->
    [< 'Token.Var (Buffer.contents buffer); lex stream >]
and lex_number buffer = parser
  | [< ''.'; stream >] ->
    Buffer.add_char buffer '.';
    lex_number_nodot buffer stream
  | [< ' ('0' .. '9') as digit; stream >] ->
    Buffer.add_char buffer digit;
    lex_number buffer stream
  | [< stream >] ->
    [< 'Token.Number (Buffer.contents buffer); lex stream >]
and lex_number_nodot buffer = parser
  | [< ' ('0' .. '9') as digit; stream >] ->
    Buffer.add_char buffer digit;
    lex_number buffer stream
  | [< stream >] ->
    [< 'Token.Number (Buffer.contents buffer); lex stream >]
and lex_char = parser
  | [< ' ('\\'); ' ('\\' | ' ' | 's' | 'n' | 'r' | 't' | 'v') as c; stream >] ->
    [< 'Token.Char ("$\\" ^ (String.make 1 c)); lex stream >]
  | [< 'c; stream >] ->
    [< 'Token.Char ("$" ^ (String.make 1 c)); lex stream >]
and lex_string buffer = parser
  | [< ' ('"'); stream >] ->
    [< 'Token.String (Buffer.contents buffer); lex stream >]
  | [< ' ('\\'); ' ('"'); stream >] ->
    Buffer.add_string buffer "\\\"";
    lex_string buffer stream
  | [< 'c; stream >] ->
    Buffer.add_char buffer c;
    lex_string buffer stream
and lex_equal = parser
  | [< ''/'; stream >] ->
    lex_not_equal stream
  | [< '':'; ''='; stream >] ->
    [< 'Token.Keyword "=:="; lex stream >]
  | [< ''='; stream >] ->
    [< 'Token.Keyword "=="; lex stream >]
  | [< stream >] ->
    [< 'Token.Keyword "="; lex stream >]
and lex_not_equal = parser
  | [< ''='; stream >] ->
    [< 'Token.Keyword "=/="; lex stream >]
  | [< stream >] ->
    [< 'Token.Keyword "=/"; lex stream >]
and lex_minus = parser
  | [< ''>'; stream >] ->
    [< 'Token.Keyword "->"; lex stream >]
  | [< stream >] ->
    [< 'Token.Keyword "-"; lex stream >]
and whitespace = parser
  | [< ' (' ' | '\r' | '\t'); stream >] -> whitespace stream

