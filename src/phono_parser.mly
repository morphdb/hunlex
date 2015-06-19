/* uses Grammar_defs Utils */
%{
module G = Grammar_defs
let cnt = ref 0
%}
  %token <string> ID
  %token EOF
  %start main 
  %type <unit> main
  %%
main:
| top { Utils.carp 1 "%d phono features declared.\n" !cnt; G.phono_cnt := !cnt }
top:
| EOF {}
| loop {}
    ;
loop: 
| ID
    { 
      Hashtbl.add G.phono_index $1 !cnt; 
      Utils.carp 3 "phono feature %d: %s\n" !cnt $1;
      incr cnt 
    }
| loop loop { }


