module Po = Program_options

let usage = ""

let parse_argv argv =
  let file_path          = ref "" in
  let indent_spaces      = ref 2 in
  let max_line_length    = ref 80 in
  let code_comment_ratio = ref 100 in
  let debug              = ref false in
  let params =
    Arg.align [ ( "-f"
		, Arg.Set_string file_path
		, "Path A file containing a list of paths to sequences (default stdin)."
                )
	      ; ( "-indent-spaces"
		, Arg.Set_int indent_spaces
		, "Int Number of spaces to indent, default is 2"
	        )
	      ; ( "-max-line-length"
		, Arg.Set_int max_line_length
		, "Int Maximum number of characters per line (default 80)"
	        )
	      ; ( "-code-comment-ratio"
		, Arg.Set_int code_comment_ratio
		, "Int Number of lines of code per 1 line of comments (default 100)"
	        )
	      ; ( "-debug"
		, Arg.Set debug
		, " Turn debugging on or off"
	        )
	      ]
  in
  Arg.parse params (fun _ -> ()) usage;
  { Po.file_path          = if !file_path = "" then None else Some !file_path
  ; Po.indent_spaces      = !indent_spaces
  ; Po.max_line_length    = !max_line_length
  ; Po.code_comment_ratio = !code_comment_ratio
  ; Po.debug              = !debug
  }

let list_of_stream stream =
  let rec list_of_stream' acc =
    match Stream.peek stream with
      | None ->
	List.rev acc
      | Some e -> begin
	Stream.junk stream;
	list_of_stream' (e::acc)
      end
  in
  list_of_stream' []

let rec print_warnings = function
  | [] -> ()
  | warning::xs -> begin
    Printf.printf "%% Line: %d\nMsg: %s\n"
      warning.Formatter.line_number
      warning.Formatter.msg;
    print_warnings xs
  end


let main argv =
  let program_options = parse_argv argv in
  let in_chan =
    match program_options.Po.file_path with
      | None -> stdin
      | Some path ->
	open_in path
  in
  let code_stream = Stream.of_channel in_chan in
  let code_lexed = list_of_stream (Lexer.lex code_stream) in
  if program_options.Po.debug then
    Token.pp_list code_lexed;
  match
    Formatter.format_to_channel
      program_options
      stdout
      code_lexed
  with
    | Return.Success [] ->
      ()
    | Return.Success warnings -> begin
      print_newline ();
      Printf.printf "%% %d warnings\n" (List.length warnings);
      print_warnings warnings
    end
    | Return.Failure error ->
      print_newline ();
      Printf.printf "%% Failed\nLine: %d\nMsg: %s\n"
	error.Formatter.line_number
	error.Formatter.msg


let () = main Sys.argv
