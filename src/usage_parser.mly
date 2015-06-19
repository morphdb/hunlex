/* uses Grammar_defs Utils */
%{

module G = Grammar_defs
let cnt = ref 0
%}
  %token <string> TAG USAGE
  %token EOF
  %start main 
  %type <unit> main
  %%
main:
| top
    {
     Utils.carp 1 "%d usage qualifiers declared.\n" !cnt; 
     G.usage_cnt := !cnt ;
   }
    ;
top:
| EOF {}
| loop {}
    ;
loop:
| USAGE TAG 
    { 
      if Hashtbl.mem G.usage_index $1 
      then  
	Utils.carp 1 
	  "Usage qualifier '%s' multiply declared. Skip this one..." $1
      else (
	Utils.carp 2 
	  "registered usage qualifier '%d': '%s' with tag '%s'\n" !cnt $1 $2;
	Hashtbl.add G.usage_index $1 (!cnt, $2)(* !cnt *); 
	G.usage_tags := $2 :: !G.usage_tags;
	incr cnt;
       )
    }
| loop loop {}

