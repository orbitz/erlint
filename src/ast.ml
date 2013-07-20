type expr = 
  | Atom of string
  | Var of string
  | Number of string
  | Binary of (operator * expr * expr)

type top_level =
  | Compiler_directive
