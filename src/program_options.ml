type t = { file_path          : string option
	 ; indent_spaces      : int
	 ; max_line_length    : int
	 (*
	  * Maximum number of lines of code per
	  * lines of comments.  Must be positive
	  * integer, larger means more code allowed
	  * per comments
	  *)
	 ; code_comment_ratio : int
	 ; debug              : bool
	 }
