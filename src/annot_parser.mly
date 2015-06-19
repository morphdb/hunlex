/* uses Grammar_defs Utils */
%{

module G = Grammar_defs
let cnt = ref 0

%}
  %token <string> TAG_TYPE
  %token EOF
  %token <int> LEVEL
  %start main 
  %type <unit> main
  %%
main:
| top
    {
     Utils.carp 1 "%d tag types declared.\n" !cnt; 
     G.tag_cnt := !cnt ;
   }
    ;
top:
| EOF {}
| loop {}
    ;
loop:
| TAG_TYPE LEVEL 
    { 
      if List.mem_assoc $1 !G.tag_index 
      then Utils.carp 1 "tag type '%s' multiply declared. Skip this one..." $1
      else (
	incr cnt;
	G.tag_index := ( $1, $2 ) :: !G.tag_index 
       )
    }
| TAG_TYPE
    { 
      if List.mem_assoc $1 !G.tag_index 
      then Utils.carp 1 "tag type '%s' multiply declared. Skip this one..." $1
      else (
	incr cnt;
	G.tag_index := ( $1, 0 ) :: !G.tag_index 
       )
    }
| loop loop {}

