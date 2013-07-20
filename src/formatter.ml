open Token

module Po = Program_options

module R = Return

type error = { line_number : int
	     ; msg         : string
	     }


type warning = error

type parser_state =
  | Top_level
  | Function

type state = { program_options       : Program_options.t
	     (* The number of indentations, not the number of spaces *)
	     ; indentation_level     : int
	     (*
	      * The amount of spaces to indent, for alignments
	      * this value includes whatever spaces are set by
	      * indentation elevel
	      * This is a list because we could have an arbitrary
	      * level of alignment for recursive elements
	      *)
	     ; alignment_spaces      : int list
	     ; alignment_spaces_curr : int
	     (* This is the current line we are reading, starting at 0 *)
	     ; src_line_number       : int
	     (* This is current line number we are writing, starting at 0 *)
	     ; dst_line_number       : int
	     ; parser_state          : parser_state list
	     ; line_builder          : Format_builder.t
	     ; builder               : Format_builder.t
	     ; warnings              : warning list
	     }

type error_t =
  | Forced_line_break
  | Msg of (state * string)

let default_state program_options =
  { program_options       = program_options
  ; indentation_level     = 0
  ; alignment_spaces      = [0]
  ; alignment_spaces_curr = 0
  ; src_line_number       = 0
  ; dst_line_number       = 0
  ; parser_state          = [Top_level]
  ; line_builder          = Format_builder.create ()
  ; builder               = Format_builder.create ()
  ; warnings              = []
  }


let string_of_token = function
  | Token.Keyword kwd -> kwd
  | Token.Atom atm    -> atm
  | Token.Var var     -> var
  | Token.Char char   -> char
  | Token.Number num  -> num
  | Token.String str  -> str
  | Token.Comment com -> "%" ^ com
  | Token.Newline     -> "\n"
  | Token.Dot         -> "."

let append_line state =
  { state with
    builder =
      (Format_builder.append
	 state.builder
	 state.line_builder);
    line_builder = Format_builder.create ()
  }

let incr ?(step = 1) num = num + step

let incr_src_line state =
  { state with src_line_number = incr state.src_line_number }


let incr_dst_line_no_append state =
  { state with
    dst_line_number = incr state.dst_line_number;
    builder = Format_builder.add_token state.builder Newline
  }

let incr_dst_line state =
  incr_dst_line_no_append (append_line state)


let (|>) d f = f d


(*
 * Kind of lame I'm naming this bind but this is effectively
 * a monad-like thing so might as well stick to the literature?
 *)
let bind f = function
  | R.Success (state, rest) ->
    f state rest
  | R.Failure error ->
    R.Failure error

let fail state msg =
  R.Failure { line_number = state.src_line_number
	    ; msg = msg
	    }

let error_of_error_t state = function
  | Msg (err_state, msg) ->
    fail err_state msg
  | _ ->
    fail state "Unknown failure"

let add_warning state msg =
  { state with
    warnings = { line_number = state.dst_line_number
	       ; msg = msg
	       }::state.warnings
  }

let add_token token state =
  { state with
    line_builder =
      Format_builder.add_token
	state.line_builder
	token
  }

let add_tokens tokens state =
  { state with
    line_builder =
      Format_builder.add_tokens
	state.line_builder
	tokens
  }

let add_space ?(num = 1) state =
  { state with
    line_builder =
      Format_builder.add_space
	state.line_builder
	num
  }


let try_catch ~succ ~fail = function
  | R.Success (state, rest) ->
    succ state rest
  | R.Failure failure ->
    fail failure

let try_catch_forced_line_break ~succ ~fail =
  try_catch
    ~succ:succ
    ~fail:(function
      | Forced_line_break ->
	fail ()
      | Msg (state, msg) ->
	fail ())


let line_length state =
  (String.length
     (Format_builder.to_string
	state.line_builder
	string_of_token))

(*
 * Sets the indentation level to whatever the
 * current line length is
 *)
let push_alignment_spaces state =
  { state with
    alignment_spaces      = (line_length state
			     - 1)::state.alignment_spaces;
    alignment_spaces_curr = line_length state - 1
  }

let pop_alignment_spaces state =
  match state.alignment_spaces with
    | [] ->
      (*
       * This is cheap, here because we can't
       * express this invariant (AFAIK) at compile time
       * in Ocaml and this *should never happen* which I
       * think is what excpetions are for.  I think
       *)
      raise (Failure "No spaces left to pop")
    | [_] ->
      raise (Failure "Cannot pop base index")
    | _::new_spaces::xs ->
      {state with
	alignment_spaces      = new_spaces::xs;
	alignment_spaces_curr = new_spaces
      }

let add_alignment state =
  add_space ~num:state.alignment_spaces_curr state

let is_line_too_long state =
  (String.length
     (Format_builder.to_string
	state.line_builder
	string_of_token)
   > state.program_options.Po.max_line_length)

let succeed state rest =
  R.Success (state, rest)


(*
 * These two functiosn exist for formatting lists of functions
 * for things like -export
 *)
let rec format_func_list_oneline state = function
    | (Keyword "]")::xs ->
      let state =
	add_token
	  (Keyword "]")
	  state
      in
      if is_line_too_long state then
	R.Failure Forced_line_break
      else
	succeed
	  state
	  xs
    | (Keyword ",")::xs ->
      format_func_list_oneline
	(state |> add_token (Keyword ",") |> add_space)
	xs
    | (Comment _)::_
    | Newline::_      ->
      R.Failure Forced_line_break
    | x::xs ->
      format_func_list_oneline
	(add_token x state)
	xs
    | [] ->
      R.Failure (Msg (state, "Unexpected end of input"))

let rec format_func_list_multiline state code =
  format_func_list_multiline_inside
    (add_space state)
    code
and format_func_list_multiline_inside state = function
  | (Keyword "]")::xs ->
    succeed
      (state
	  |> incr_dst_line
	  |> add_alignment
	  |> add_token (Keyword "]")
	  |> pop_alignment_spaces)
      xs
  | (Keyword ",")::(Comment com)::xs ->
    (*
     * We have a situation like:
     * [ foo, % comment about foo
     *   bar, % comment about bar
     * ]
     *
     * Which we want to turn into
     * [ foo % Comment about foo
     * , bar % comment about bar
     * ]
     *)
    format_func_list_multiline_inside
      (state |> add_space |> add_token (Comment com))
      (*
       * Toss the ',' back on list
       *)
      ((Keyword ",")::xs)
  | (Keyword ",")::Newline::Newline::xs ->
    format_func_list_multiline_inside
      (state
	  |> incr_src_line
	  |> incr_dst_line)
      ((Keyword ",")::Newline::xs)
  | (Keyword ",")::Newline::(Comment com)::xs ->
    format_func_list_multiline_inside
      (state
	  |> incr_src_line
	  |> incr_dst_line
	  |> add_alignment
	  |> add_token (Comment com))
      ((Keyword ",")::xs)
  | (Keyword ",")::Newline::xs ->
    format_func_list_multiline_inside
      (state |> incr_src_line)
      ((Keyword ",")::xs)
  | (Keyword ",")::xs ->
    format_func_list_multiline_inside
      (state
	  |> incr_dst_line
	  |> add_alignment
	  |> add_token (Keyword ",")
	  |> add_space)
      xs
  | Newline::(Comment com)::xs ->
    format_func_list_multiline_inside
      (state
	  |> incr_dst_line
	  |> incr_src_line
	  |> add_alignment
	  |> add_token (Comment com))
      xs
  | (Comment com)::xs ->
    format_func_list_multiline_inside
      (state
	  |> add_space
	  |> add_token (Comment com)
	  |> add_alignment)
      xs
  | Newline::xs ->
    format_func_list_multiline_inside
      (state |> incr_src_line)
      xs
  | x::xs ->
    format_func_list_multiline_inside
      (state |> add_token x)
      xs
  | [] ->
    R.Failure (Msg (state, "Unexpected end of input"))



let rec format_top_level state = function
  | Newline::xs ->
    format_top_level (incr_dst_line (incr_src_line state)) xs
  | (Comment text)::xs ->
    let state =
      add_token
	(Comment text)
	state
    in
    if is_line_too_long state then
      format_top_level
	(add_warning state "Comment longer than maximum line length")
	xs
    else
      format_top_level state xs
  | (Keyword "-")::(Atom "module")::xs ->
    bind
      format_top_level
      (format_module
	 (add_tokens
	    [Keyword "-"; Atom "module"]
	    state)
	 xs)
  | (Keyword "-")::(Atom "export")::xs ->
    bind
      format_top_level
      (format_export
	 (add_tokens
	    [Keyword "-"; Atom "export"]
	    state)
	 xs)
  | [] ->
    succeed (append_line state) []
  (* TODO: This needs to be removed at some point *)
  | _::xs -> format_top_level state xs
and format_module state = function
  | (Keyword "(")::(Atom mod_name)::(Keyword ")")::Dot::xs ->
    succeed
      (add_tokens
	 [Keyword "("; Atom mod_name; Keyword ")"; Dot]
	 state)
      xs
  | _ ->
    fail state "Unknown -module declaration"
and format_export state = function
  | (Keyword "(")::(Keyword "[")::xs -> begin
    let state =
      add_tokens
	[Keyword "("; Keyword "["]
	state
    in
    try_catch
      ~succ:(fun state rest ->
	bind
	  format_top_level
	  (format_export_end
	     state
	     rest))
      ~fail:(fun _ ->
	try_catch
	  ~succ:(fun state rest ->
	    bind
	      format_top_level
	      (format_export_end
		 state
		 rest))
	  ~fail:(error_of_error_t state)
	  (format_func_list_multiline
	     (push_alignment_spaces state)
	     xs))
      (format_func_list_oneline
	 state
	 xs)
  end
  | _ ->
    fail state "Unknown -export declaration"
and format_export_end state = function
  | (Keyword ")")::Dot::xs ->
    succeed
      (state |> add_tokens [Keyword ")"; Dot])
      xs
  | _ ->
    fail state "Unexpected tokens in -export"


let format_code program_options code =
  (*
   * Initiate the buffer at the number of tokens * 10, just
   * a reasonable round number
   *)
  let state = default_state program_options in
  match format_top_level state code with
    | R.Success (state, []) ->
      let code =
	Format_builder.to_string
	  state.builder
	  string_of_token
      in
      R.Success (code, state.warnings)
    | R.Success (state, _) ->
      fail state "Did not consume entire input"
    | R.Failure error ->
      R.Failure error

let format_to_channel program_options out_chan code =
  match format_code program_options code with
    | R.Success (formatted_code, warnings) -> begin
      output_string out_chan formatted_code;
      R.Success warnings
    end
    | R.Failure errors ->
      R.Failure errors
