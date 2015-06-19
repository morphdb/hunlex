(* File lexer.mll *)
{
 module P = Kr_parser 
}
let identchar = [^'<' '>' '|' '\n' ';' ',' '+' '=' '\r' ' ' '\t' '?']
let ws = [' ' '\t' ]
let eop = ['\n' '\r']+
rule token = parse
| ws+              { token lexbuf }     (* skip blanks *)
| eop? ws* eof     { Utils.carp 3 "EOF "; P.EOP }
| eop              { Utils.carp 3 "EOP "; P.EOP }
| ':'              { Utils.carp 3 "DEF "; P.DEF }
| '<'              { Utils.carp 3 "OPEN "; P.OPEN }
| '>'              { Utils.carp 3 "CLOSE "; P.CLOSE }
| '?'              { Utils.carp 3 "MAY "; P.MAY }
| identchar+ as x  { Utils.carp 3 "EDGE(%s)" x; P.EDGE(x) }


