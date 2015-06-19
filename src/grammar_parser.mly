/* uses Grammar_defs Printf */
%{

module G = Grammar_defs 

let rule_cnt = G.rule_cnt
let rules = G.rules

let parse_error string = 
  Utils.carp 0 "Grammar_parser: Parse error: %s\n" string;
  raise Parsing.Parse_error

%}
  %token <Grammar_defs.outfunc> OUTFUNC
  %token INI FIN FORALL LPAREN RPAREN DELIM END EOF NOT ZERO SKIP
  %token <Kr.t option> FS
  %token <string> DEFINE REGEXP REGEXP_STRING COMPOUND_MOD COMPOUND_CAT
  %token <string> PREFIX SUFFIX SUBST LEXIS SUPPL
  %token <string * string> CIRCUMFIX
  %token <bool> FREE PASS
  %token <Grammar_defs.morph> MORPH 
  %token <int> PHONO CLIP PHONO_COND USAGE FILTER KEEP RECURSION
  %token <Grammar_defs.pattern> MATCH 
  %token <int * string> TAG_STRING USAGE
  %start main 
  %type <Grammar_defs.rule list * int> main
  %start entry
  %type <Grammar_defs.morph option> entry
  %%
main:
| loop                              { !rules, !rule_cnt }
    ;
loop:
| entry loop                        {  }
| EOF                               {  }

    ;
  entry:
| COMPOUND_MOD COMPOUND_CAT END
    {
     let get_flag x = 
       try 
	 Hashtbl.find G.compound_flag_tbl x
       with Not_found ->
	 let mod_flag = ref "" in
	 Hashtbl.add G.compound_flag_tbl x mod_flag;
	 mod_flag
     in
     let mod_flag, cat_flag = get_flag $1, get_flag $2 in
     Utils.lazy_carp 4  (lazy (Printf.eprintf "(pattern '%s+%s' added to compound patterns)\n" $1 $2));
     G.compound_patterns := (mod_flag, cat_flag) :: !G.compound_patterns;
     None
   }
| REGEXP REGEXP_STRING END
    { 
      Hashtbl.add G.pat_index $1 $2;
      Utils.lazy_carp 3 (lazy (Printf.eprintf "declare regexp pattern '%s'\n" $2) );
      None
    }

/* this should be allowed if some id-s are skipped */
/* note that references to such skipped macros will 
fall back to morphs and since they are not included, will be skipped */
| DEFINE END {
  Utils.lazy_carp 2 (lazy (Printf.eprintf "Warning: definition of '%s' skipped\n" $1));
  None
}
| definition END  
    { 
      match $1 with macro, definition -> 
	Hashtbl.add G.abbr_index macro definition; 
	Utils.lazy_carp 3 (lazy (Printf.eprintf "macro '%s' defined\n" macro));
	None
    }

/* inert morpheme, one without rules */
| preamble rules 
    {
     match $1 with morph, morph_desc ->
       (* features to be filtered but globally set as positive output must be ignored
	  this also means that the output for all features to be filtered are unset
	  in morph_desc._bv and therefore only positive output is to be set 
	  - this is not true of the conditions 
	  (find all filtered that is not set as positive in output)
	*)
       let features = List.find_all 
	   (fun i -> not (Bv.get morph_desc.G._bv i))
	   morph_desc.G._filter in
       (* set all filtered features as negative conditions on the global morph *)
       let morph_desc = Utils.fold_left (G.set_neg_phono_cond true) morph_desc features in

       (* this is a function of each non-zero subset of the filtered features *)
       let apply subset rulez = 
	 (* set the subset of features as positive conditions *)
	 let desc = G.make_desc ~desc:morph_desc () in 
	 desc.G._level <- morph.G.level;
	 desc.G._recursion <- morph.G.recursion_limit;
	 desc.G._no_stem <- morph.G.no_stem;
	 let desc  = Utils.fold_left (G.set_phono_cond) desc subset in
	 (* the rest as negative conditions
	    this is important in order to inherit a feature if and only if 
	    it is present in the input 
	  *)
	 let desc   = Utils.fold_left (G.set_neg_phono_cond false) desc subset in
	 (* and as output conditions *)
	 let desc  = Utils.fold_left (G.set_phono true) desc subset in
	 (* create the rule from the desc *)
	 Utils.carp 5 "Added a filter rule\n";
	 let id = !rule_cnt  in
	 incr rule_cnt;
  	 G.add_rule morph desc id rulez
       in
       (* fold on the non-empty powerset of the feature set 
	  
	* for one defaulted feature this will give one rule
	* for no defaulted feature this will give no rule
	* for n defaulted features this will give 2^n - 1 rules
	  
	*)

       rules := Utils.fold_on_powerset (apply) !rules features;
       
       (* every rule has resulted in a desc->desc function *)
       (* this function (for folding) takes the desc function 
	  to create a rule to add to the morphs rules *)
       let apply rulez (desc_func)  = 
	 let init_desc = G.make_desc ~desc:morph_desc () in 
	 init_desc.G._level <- morph.G.level;
	 init_desc.G._recursion <- morph.G.recursion_limit;
	 init_desc.G._no_stem <- morph.G.no_stem;
	 (* make_desc involves copying *)
	 let desc = desc_func init_desc in
	 let id = !rule_cnt in
	 incr rule_cnt;
	 G.add_rule morph desc id rulez
       in 
       (* fold over these desc functions create the rule *)
       rules := List.fold_left (apply) !rules $2;
       (* rules := List.rev_append morph.G.rules !rules; *)
       Some(morph)
   }
    ;
  rules:
| END              { [] }
| rule rules  { $1 :: $2 }
    ;
  definition:
| DEFINE            { $1, fun x -> x }
| definition desc        { match $1 with macro, func -> macro, ( fun x -> $2 (func x) ) }
    ;
  preamble:
| MORPH            { $1, G.make_desc () }
| preamble desc    { match $1 with morph, d -> morph, $2 d }
    ;

  rule:
| DELIM            { fun x -> x }
| rule desc        { fun x -> $2 ($1 x) }
    ;
  desc:
| PREFIX  
    {
     G.set_prefix $1
   }
| SUBST
    {
     G.set_subst $1
   }
| SUPPL
    {
     G.set_lexis $1 (* sic! *)
   }
| LEXIS
    {
     G.set_lexis $1
   }
| SUFFIX  
    {
     G.set_suffix $1
   }
| ZERO  
    {
     G.set_zero 
   }
| CIRCUMFIX
    {
     G.set_circumfix $1
   }
| NOT FS 
    {
     G.set_fs $2
}
| FS 
    {
     G.join_fs $1
   }
| PHONO 
    { 
      G.set_phono true $1
    }
| NOT PHONO 
    { 
      G.set_phono false $2
    }
| FREE
    { G.set_free $1 }
| PASS
    { G.set_pass $1 }
| MORPH  
    { 
      G.add_morph $1
    }
| NOT MORPH 
    { 
      G.remove_morph $2
    }
| NOT 
    {
     fun x -> x 
   }
| OUTFUNC 
    { 
      $1
    }
| NOT TAG_STRING 
    {
     G.set_tag $2
   }
| TAG_STRING 
    {
     G.add_tag $1
   }
| CLIP 
    {
     G.set_clip $1
   }
| INI CLIP 
    {
     G.set_clip_ini $2
   }
| FIN CLIP 
    {
     G.set_clip_fin $2
   }
| MATCH 
    {
     G.set_match $1
   }
| FORALL MATCH 
    {
     G.set_match_forall $2
   }
| INI MATCH  
    {
     G.set_match_ini $2
   }
| FIN MATCH  
    {
     G.set_match_fin $2
   }
| PHONO_COND 
    {
     G.set_phono_cond $1 
   }
| NOT PHONO_COND 
    {
     G.set_neg_phono_cond true $2
   }
| KEEP 
    {
     G.set_keep true $1
   }
| NOT KEEP 
    {
     G.set_keep false $2
   }
| FILTER 
    {
     G.set_filter $1
   }
| RECURSION 
    {
     G.set_recursion $1
   }
| USAGE 
    {
     G.set_usage true $1
   }
| NOT USAGE 
    {
     G.set_usage false $2
   }
    ;



