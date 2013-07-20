type error = { line_number : int
	     ; msg : string
	     }

type warning = error

val format_code       : Program_options.t -> Token.t list -> ((string * warning list), error) Return.t
val format_to_channel : Program_options.t -> out_channel -> Token.t list -> (warning list, error) Return.t
