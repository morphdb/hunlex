/* uses Grammar_defs Utils */
%{

module G = Grammar_defs
let cnt = ref 0
let max_level = ref 0
%}
  %token <string> AFFIX
  %token EOF
  %token <int> LEVEL
  %start main 
  %type <unit> main
  %%
main:
| top
    {
     Utils.carp 1 "%d morphemes declared.\n" !cnt; 
     G.morph_cnt := !cnt ;
   }
    ;
top:
| EOF {}
| loop {}
    ;
loop:
| AFFIX LEVEL LEVEL
    { 
      let level = 
	if $2 <= !G.min_level then (!G.min_level)
	else if $2 < !G.max_level then $2
	else !G.max_level
      in
      if Hashtbl.mem G.morph_index $1 
      then Utils.carp 1 "'%s' multiply declared. Skip this one..." $1
      else (
	let no_stem = if $3 = 0 then false else true in 
	  let morph = G.make_morph $1 !cnt level no_stem in 
	  Utils.carp 2 "created morph node %d: %s\n" !cnt $1;
	  incr cnt;
	  Hashtbl.add G.morph_index $1 morph; 
       )
    }
| AFFIX LEVEL 
    { 
      let level = 
	if $2 <= !G.min_level then (!G.min_level)
	else if $2 < !G.max_level then $2
	else !G.max_level
      in
      if Hashtbl.mem G.morph_index $1 
      then Utils.carp 1 "'%s' multiply declared. Skip this one..." $1
      else (
	  let morph = G.make_morph $1 !cnt level false in 
	  Utils.carp 2 "created morph node %d: %s\n" !cnt $1;
	  incr cnt;
	  Hashtbl.add G.morph_index $1 morph; 
       )
    }
| AFFIX  
    { 
      let level = !G.max_level in
      if Hashtbl.mem G.morph_index $1 
      then Utils.carp 1 "'%s' multiply declared. Skip this one..." $1
      else (
	let morph = G.make_morph $1 !cnt level false in 
	Utils.carp 2 "created morph node %d: %s\n" !cnt $1;
	incr cnt;
	Hashtbl.add G.morph_index $1 morph; 
       )
    }
| loop loop {}

