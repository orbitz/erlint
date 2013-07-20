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


let token_to_string = function
  | Keyword kw ->
    Printf.sprintf "Keyword %s" kw
  | Atom atom ->
    Printf.sprintf "Atom %s" atom
  | Var var ->
    Printf.sprintf "Var %s" var
  | Char c ->
    Printf.sprintf "Char %s" c
  | Number num ->
    Printf.sprintf "Number %s" num
  | String str ->
    Printf.sprintf "String \"%s\"" str
  | Comment comment ->
    Printf.sprintf "Comment \"%s\"" comment
  | Newline ->
    "Newline"
  | Dot ->
    "Dot"

let rec pp_stream stream =
  match Stream.peek stream with
    | None ->
      Printf.printf "\n"
    | Some Newline -> begin
      Printf.printf "(Newline)\n";
      Stream.junk stream;
      pp_stream stream
    end
    | Some token -> begin
      Printf.printf "(%s) " (token_to_string token);
      Stream.junk stream;
      pp_stream stream
    end

let rec pp_list = function
  | [] ->
    Printf.printf "\n";
  | Newline::xs -> begin
    Printf.printf "(Newline)\n";
    pp_list xs
  end
  | x::xs -> begin
    Printf.printf "(%s) " (token_to_string x);
    pp_list xs
  end
