(* uses Morph_parser *)
{
module P = Morph_parser
exception Eof

}
let identchar = [^'"' '#' ' ' '\t' '\r' '\n' ';' ',' '+' '=']
let id = (identchar)+
let number = ['0'-'9']+
rule token = parse 
  [' ' '\t' '\n' '\r']+     { token lexbuf }     (* skip blanks *)
| '#'[^'\n']*'\n'?      { token lexbuf }     (* skip comments *)
| number as x          { P.LEVEL(int_of_string x) }
| id as x              { P.AFFIX(x) }
| eof { P.EOF }
