(* uses Phono_parser Lexing *)
{
module P = Phono_parser
}
let identchar = [^'"' '#' ' ' '\t' '\n' ';' ',' '+' '=' '\r']

rule token = parse 
  [' ' '\t' '\n' '\r']+    { token lexbuf }     (* skip blanks *)
| '#'[^'\n']*'\n'      { token lexbuf }     (* skip comments *)
| identchar+ as x 
    { 
      P.ID(x)
    }
| eof { P.EOF }
