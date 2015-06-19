(* uses Usage_parser *)
{
module P = Usage_parser
exception Eof
type state = Usage | Tag
let state = ref Usage
}
let identchar = [^'"' '#' ' ' '\t' '\r' '\n' ';' ',' '+' '=']
let id = (identchar)+
rule token = parse 
  [' ' '\t']+     { token lexbuf }     (* skip blanks *)
| '#'[^'\n']*'\n'?      { token lexbuf }     (* skip comments *)
| [ '\n' '\r']+         { if !state = Tag then (state := Usage; P.TAG("")) else token lexbuf }
| id as x              { if !state = Usage then (state := Tag; P.USAGE(x)) else (state := Usage; P.TAG(x)) }
| eof { P.EOF }
