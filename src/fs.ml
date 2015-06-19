
exception Compatibility_undefined
exception FS_parsing_error

let signature = Kr.sig_make()

let read_buffer buffer = 
  try 
    let avsf = Kr_parser.main Kr_lexer.token buffer in
    avsf signature
  with Parsing.Parse_error
    -> Utils.carp 0 "Kr parsing error caught\n";
      raise FS_parsing_error

let read string =
  try
    let buffer = Lexing.from_string string in
    read_buffer buffer
  with FS_parsing_error as e 
    -> Utils.carp 0 "Error parsing fs string '%s'\n" string;
      raise FS_parsing_error

let join_or_unify f x y = 
  match x, y with 
  | None, None -> None
  | s, None -> s
  | None, s -> s
  | Some(fs1), Some(fs2) -> Some(f fs1 fs2)

let unify = join_or_unify Kr.unify

let join = join_or_unify Kr.join

let print x = 
  match x with 
  | None -> "None" 
  | Some(fs) -> Kr.print fs

let compatible x y = 
  Utils.carp 4 "compatible?\n\t%s\n\t%s\n" (print x) (print y);
  match x, y with 
  | Some(fs1), Some(fs2) 
    -> ( try 
      let j = Kr.join fs1 fs2 in 
      let _ = Kr.unify fs1 j in
      Utils.carp 3 "Yes";
      true
    with Kr.Buttom -> 
      Utils.carp 3 "No";
      false
	)
  | _, _ -> raise Compatibility_undefined


