type t

val create      : unit -> t
val add_token   : t -> Token.t -> t
val add_tokens  : t -> Token.t list -> t
val add_space   : t -> int -> t
val append      : t -> t -> t
val to_string   : t -> (Token.t -> string) -> string
