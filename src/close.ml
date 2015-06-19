(* uses Utils Grammar_defs Output Printf Str *)

module G = Grammar_defs

exception No_match

exception Zero

exception Illegal_combination 

exception Oops

(* only one global for no continuation 
   should this be a nullable?
*)
let global_no_continuation = 0, G.IndexSet.empty, ref 0, ref ""

let global_true_ref = ref true 
let global_0_ref = ref 0
let global_empty_string_ref = ref ""

(* ones with no continuation are not stored in the index hash, have a global ref *)

let circ_cnt = ref 0
let index_set_cnt = ref 0 
let get_cont index_set = 
  try 
    Hashtbl.find G.cont_tbl index_set
  with Not_found -> 
    let flag_str_ref = ref "" in
    let freq_ref = ref 0 in
    incr index_set_cnt;
    let cont = !index_set_cnt, index_set, freq_ref, flag_str_ref in
    Hashtbl.add G.cont_tbl index_set cont;
    cont
      
let get_aff_output active_out = 
  function G.Aff_output (a,t,fs,free,circ,cont) as aff_output ->
    (* the outfix hash is indexed with aff_output
       structures.
     * continuations should be merged for indentical lexis 
     * and free flag bool or-ed 
     *)
    let _, index_set, freq_ref, flag_str_ref = !cont in
    match a with 
    | G.Lexis x -> (
	let _ = 
	  try
	    (* lexis that only differ in continuations and freeness 
	       are considered equivalent
	     * zero index-set index is not active since it is for 
	       no continuations 
	     * the dic_tbl hash stores references to 
	       freeness and the continuation set *)
	    let free', cont' = G.Dic_tbl.find G.dic_tbl aff_output in
            free' := (!free' || !free);
	    (* if the continuations are not identical *)
            if not (!cont' == !cont)
	    then ( 
	      let _, index_set', freq_ref', flag_str_ref' = !cont' in
	      (* merge the indexsets *)
	      let new_index_set = G.IndexSet.union index_set index_set' in
	      (* decrement earlier index_set frequency *)
	      decr freq_ref'; 
	      (* register a new index set and assign it to the old outfix *)
	      let cont = get_cont new_index_set in	      
	      cont' := cont;
	      (* increment the new index set's ref count *)  
	      let _, _, freq_ref, _ = cont in
	      incr freq_ref;
	     );
	  with Not_found -> 
	    (* if the same lexis is not found then
	     * increment dic output count as well as 
	       ref count of the index set *)
	    Utils.lazy_carp 6 (lazy (Printf.eprintf "%s\n" x; flush stderr));
	    incr G.dic_output_count;
	    incr freq_ref;
	    (* register the output in the dic_tbl hash *)
	    G.Dic_tbl.add G.dic_tbl aff_output (free, cont);
	in
	(* lexis have the zero outfix index and irrelevant ref count *)
	global_0_ref, G.Outfix ( aff_output, global_0_ref, global_0_ref )
       )
    | _ -> (
	(* for affixes *)
	try 
	  (* affixes are identical only if continuations are identical *)
          let outfix = G.Aff_tbl.find G.aff_tbl aff_output in
	  let index = match outfix with G.Outfix ( _, index, freq_ref ) -> index in
	  index, outfix
	with Not_found ->
	  (* create a new outfix with 0 ref index *)
	  let index = if active_out then (
	    incr G.aff_output_count;
	    G.aff_output_list := aff_output :: !G.aff_output_list;
	    incr freq_ref;
	    ref !G.aff_output_count
	   ) else ref 0
	  in
	  let outfix = G.Outfix ( aff_output, index, freq_ref ) in
	  (* register the new outfix in the aff_tbl hash *)
	  G.Aff_tbl.add G.aff_tbl aff_output outfix;
	  index, outfix
       )

(* matching functions are rather clumsy *)	    
let match_start re s = 
  Str.string_match re s 0

(* this is the most disgusting bit and should be changed *)
let match_end re s = 
  let len = (String.length s) in
  len <> 0 && (
  try ignore ( Str.search_backward re s ( len ) ); 
    let last = Str.match_end () in
    last = len
  with Not_found -> false
 )

(* combine two affix type structures *)
let rec combine_func_make affix = 
  match affix with 
  | G.Prefix(app0, clip0, pattern0) -> 
      let rec pref_func x = 
	match x with 
	| G.Prefix(app1, clip1, pattern1) -> begin
	    match pattern1 with 
	    | None -> 
		G.Prefix(
		app1 ^ String.sub app0 clip1 (String.length app0 - clip1),
		clip0,
		pattern0)
	    | Some(_,re) ->
		if match_start re app0 (* this matches beginning ????*) 
		then G.Prefix(
		  app1 ^ String.sub app0 clip1 (String.length app0 - clip1),
		  clip0,
		  pattern0)
		else raise No_match
	end
	| G.Circumfix(p,s) ->
	    G.Circumfix(pref_func p,s)
	| G.Suffix(_,_,_) as s ->
	    G.Circumfix(affix,s)
	| G.Subst(_,_,_) -> 
	    Utils.carp 0 "Error: Close: combine: prefix -> subst is uninterpretable\n"; 
	    raise Illegal_combination 
	| G.Lexis(_) -> 
	    Utils.carp 0 "Error: Close: combine: prefix -> lexis is uninterpretable\n"; 
	    raise Illegal_combination 
	| G.Dummy(_) as s -> s
      in pref_func
  | G.Suffix(app0, clip0, pattern0) -> 
      if app0 = "" && clip0 = 0 && pattern0 = None then raise Zero else (
      let rec suff_func x =  
	match x with 
	| G.Suffix(app1, clip1, pattern1) as s -> (
	    try
	      let matched = match pattern1 with 
	      | None -> true
	      | Some(_,re) -> match_end re app0 
	      in
              if not matched then raise No_match;
	      let clipped = match clip1 with 
	      | 0 -> app0
	      | _ -> String.sub app0 0 (String.length app0 - clip1)
	      in
	      let affixed = match app1 with 
	      | "" -> clipped (* truncation *)
	      | _ -> clipped ^ app1
	      in
	      Utils.lazy_carp 6 (lazy (Printf.eprintf "%s\n" affixed));
	      G.Suffix(affixed, clip0, pattern0)
	    with Invalid_argument "String.sub" -> 
	      Utils.carp 0 "unable to clip %d characters from '%s'\n" clip1 app0;
	      raise (Failure "string too short")
	   )
	| G.Circumfix(p,s) ->
	    G.Circumfix(p,suff_func s)
	| G.Prefix(_,_,_) as p ->
	    G.Circumfix(p,affix)
	      (*	
		 | G.Subst(_,_,_) -> 
		 Utils.carp 0 "Error: Close: combine: suffix -> subst is uninterpretable\n"; 
		 raise Illegal_combination
	       *)
	| G.Subst(templ,forall,pattern) -> ( 
	    match pattern with 
	    | None -> 
		Utils.carp 1 "WARNING: suffix suppletion: substitute %s -> %s\nIt is unlikely you really want this\n" app0 templ;
		G.Suffix(templ,clip0,pattern0)
	    | Some(re_s,re) -> (
		let result = Str.global_replace re templ app0 in
		if forall || result <> app0 then G.Suffix(result,clip0,pattern0)
		else raise No_match
	       )
	   )
	| G.Lexis(_) -> 
	    Utils.carp 0 "Error: Close: combine: suffix -> lexis is uninterpretable\n";
	    raise Illegal_combination
	| G.Dummy(_) as s -> s
       
      in suff_func
     )
  | G.Circumfix(p,s) ->
      let pref_func = combine_func_make p
      and suff_func = combine_func_make s in
      let circ_func x = 
	match x with 
	| G.Dummy(_) as s -> s
	| G.Prefix(_,_,_) as p1 -> G.Circumfix(pref_func p1,s)
	| G.Suffix(_,_,_) as s1 -> G.Circumfix(p,suff_func s1)
	| G.Circumfix(p1,s1) -> G.Circumfix(pref_func p1,suff_func s1)
	| G.Subst(_,_,_) as s1 -> 
	    Utils.carp 2 "NB: combine on circumfix -> subst is interpreted only for suffix component\n"; 
	    G.Circumfix(p,suff_func s1)
	| G.Lexis(_) -> 
	    Utils.carp 0 "Error: Close: combine: circumfix -> lexis is uninterpretable\n"; 
	    raise Illegal_combination
      in circ_func
  | G.Lexis(w) ->
      let rec lex_func x = 
	match x with 
	| G.Dummy(s) -> (
	    match s 
	    with G.Lexis(_) -> s 
	    | _ -> lex_func s
	   )
        | G.Prefix(app1,clip1,pattern1) -> begin
	    match pattern1 with 
	    | None -> G.Lexis(app1 ^ String.sub w clip1 (String.length w - clip1) )
	    | Some(_,re) ->
		if match_start re w  
		then G.Lexis(app1 ^ String.sub w clip1 (String.length w - clip1) )
		else raise No_match
	end
	| G.Suffix(app1,clip1,pattern1) -> begin
	    match pattern1 with 
	    | None -> G.Lexis(String.sub w 0 (String.length w - clip1) ^ app1)
	    | Some(_,re) ->
		if match_end re w  
		then G.Lexis(String.sub w 0 (String.length w - clip1) ^ app1)
		else raise No_match
	end
	| G.Circumfix(p,s) -> 
	    combine_func_make (lex_func p) s
	| G.Subst(templ,forall,pattern) -> begin 
	    match pattern with 
	    | None -> 
		G.Lexis(templ) 
	    | Some(re_s,re) -> (
		(* Utils.carp 3 "subst '%s' '%s' '%s'" re_s templ w; *)
		let result = Str.global_replace re templ w in
		if forall || result <> w then G.Lexis(result) else raise No_match
	       )
	end
	| G.Lexis(_) as l -> 
	    Utils.carp 0 "Error: Close: combine: lexis -> lexis is uninterpretable (it shouldn't be)\n"; 
	    raise Illegal_combination

      in lex_func
  | G.Subst(templ,_,pattern) as y -> (
      match pattern with None -> (* if suppletion *)
	combine_func_make ( G.Lexis(templ) )
      | _ -> 
	  let rec subst_func x = 
	    match x with 
	    | G.Dummy(_) as s -> s
	    | _ -> Utils.carp 0 "Error: Close: combine: subst -> x is uninterpretable\n"; 
		raise Illegal_combination
	  in
	  subst_func
     )
  | G.Dummy(d) -> 
      let inside_func = combine_func_make d in
      let dummy_func x = G.Dummy(inside_func x) in 
      dummy_func
	

let satisfies features rule = 
  (*Utils.carp 3 "%s\n" (Bv.to_string features);*)
  let pos, neg = rule.G.conditions, rule.G.neg_conditions in
  let comp = Bv.po_compare features pos  in 
  let res = (comp = Bv.Lower || comp = Bv.Equal)
      &&
    (try let _ = Bv.intersect features neg in false with Bv.All_zero -> true)
  in
  (* Utils.carp 4 "%s satisfies conditions %s %s? %b\n"
    (Bv.to_string features) (Bv.to_string pos) (Bv.to_string neg) res; *)
  res
  

let precompile_out rule path (close_func) = 
  match !(rule.G.output) with 
  | G.Processed(aff_list, cont) -> 
      (* Utils.carp 3 "preprocessed continuations for '%s'\n" rule.G.morph.G.name; *)
      aff_list, cont
  | G.Unprocessed( level , out_key, out_bv) -> (
      (* Utils.carp 3 "processing continuations for prototype '%s'\n" rule.G.morph.G.name; *)

      (* function for folding over rules (continuations) *)
      let rule_fold_func features (aff_list, continuations) rule = 
	if ( satisfies features rule ) 
	then (
	  (* features specified satisfy conditions *)
	  (* Utils.carp 3 "Features satisfied\n"; *)
	  let rule = match rule.G.keep with 
	  | Some(keep) -> (
	      (* no clone is ever virtual again *)
	      (* the clone is not entered on the morphs rule-list, 
		 so will not be hit upon 
		 next time the morph is encountered
		 but 
	       * will be entered on the affix list after it is compiled so 
		 it closes as any other rule 
	       * entered on the clone hash so it is closed only once for each 
		 combination of features that are to be kept
	       * entered on the rule list for which the flags are calculated
	       *)
	      
	      (* we have to include the clone in the calculations but leave the 
		 master rule on the morph list, so each time a new combination
		 a clone is checked. 
		 Clones are recorded on the master indexed by the 
		 features inherited 
		 (intersection of keep with positive conditions)
		 if a clone is retrieved, closure is recursively called on it.
	       *)
	      let pos = Bv.bw_and keep features in
	      let id = rule.G.rule_id in
	      try 
		    (* Utils.carp 4 "lookup clone...";*)
		Hashtbl.find G.clone_tbl (id, pos);
	      with Not_found ->
		let neg = Bv.bw_and keep (Bv.bw_not features) in
		let clone = G.make_clone rule pos neg in
		(* Utils.carp 4 "created clone\n"; *)
		Hashtbl.add G.clone_tbl (id, pos) clone;
		(* G.rules := clone :: !G.rules; *)
		clone
	     )
	  | None -> rule (* no clones *)
	  in
	  if level >= rule.G.rule_level
	  then 
	    (* for rules on identical or lower level, add to aff_list *)
	    (rule :: aff_list, continuations)
	  else 
	    (* for rules on identical or lower level, add to continuation list *)
	    (aff_list, rule :: continuations)
	 ) else 
	  (* features specified do not satisfy conditions *)
	  (aff_list, continuations)
      in

      let morph_fold_func (aff_list, continuations) (morph, _) = 
	(* Utils.carp 3 "launch --> %s\n" 
	morph.G.name;*)
	List.fold_left (rule_fold_func out_bv) 
	  (aff_list, continuations) 
	  morph.G.rules
      in

      let aff_list, continuations = 
	List.fold_left (morph_fold_func) 
	  ([], [])
	  out_key
      in
      
      (* Utils.carp 4 "register precompiled final affixlist\n" ;*)
      (* output is shared between rules having the same level & output *)

      (* setting flag_string_refs for output flags *)
      let cont = 
	(* register new continuations in flag hash table *)
	if continuations = []
	then (
	  (* Utils.carp 5 "no continuations\n"; *)
	  global_no_continuation
	 ) else (
	  try 
	    (* Hashtbl.find G.flag_tbl continuations *)
	    raise Not_found
	  with Not_found -> (
	    let outfix_record_func list = function G.Outfix( a, index_ref, freq_ref ) -> 
	      if !index_ref = 0 then (
		(* this affix rule is active as an output, so we 
		   register it as an outfix *)
		(* circumfixes are handled here *)
		
		match a with 
		  G.Aff_output(G.Circumfix(p,s),tag,fs,free,circ,cont) -> (
		    (* make sure that the suffix part of a circumfix is also 
		       registered as separate suffix, even if it is identical to 
		       a non-bound suffix 
		   * Dummy suffix construct is used to distinguish the suffix component
		     of a circumfix from an identical suffix 
		   * but we need to create a new indexlist flag_str stuff to 
		     to register the prefix continuations as well 
		   * if we register a new flag_str for every circumbound suffix
		     (leaving the suffixes flag as well), it will have the 
		     effect of assigning the circumbound prefixes to separate flags 
		     which is needed anyway.
		     However, the indexlist must be available with the flag
		     to preserve the other continuations to the circumbound suffix
		     also one need to accumulate extra flags 
		     it is enough to have a hash (aff_output -> index_list ref, flag)
		     which is also folded when flags are allocated and 
		     the flag_str as an extra ingredient is recorded on aff_output
		     since flags_str is empty then all circumbound suffixes identical 
		     will be hashed only once (in outfix_tbl)
		     if it is made a string ref option, one can even distinguish
		     pure suffixes and circumbound suffixes without a resort to 
		     the Dummy construct, which is a nicer solution
		     so clear TODO: 
		     grammar_defs:
		     extend aff_output adding a string ref option as 6th element
		     store extra_components in an aff_output indexed hash
		     its indexlist is dynamically extendable as new circumfixes are 
		     found having the same circumbound suffix therefore the hash 
		     should give indexlist ref 
		     flag: in order for these index+flagstr to be processed by flag 
		     they should also be folded to components in flag.ml
		     output: dump the extra flag string together with a circumfix flag
		   *)
		    
		    let suffix_output = 
		      G.Aff_output(s,tag,fs,free,ref true,cont)
		    in
		    
		    
		    let i, _ = 
		      get_aff_output true suffix_output  
		    in
		    (* let _ = G.IndexSet.iter (Utils.carp 0 "%d ") !(!index_list) in *)
		    (* Utils.carp 0 "\n"; *)
		    let new_index_list = G.IndexSet.singleton !i in
		    let prefix_output = 
		      (* G.Aff_output(p,"",None,ref true,ref true,ref (ref G.IndexSet.empty),ref (ref "")) *)
		      let cont = 
			get_cont new_index_list
		      in
		      G.Aff_output(p,"",None, ref false,ref true, ref cont)
		    in
		    let i, _ = 
		      get_aff_output true prefix_output
		    in
		    (* the circumfix will have the same output index 
		       as the prefix itself *)
		    index_ref := !i;
		    
		   )
		| _ -> 
		    incr G.aff_output_count;
		    incr freq_ref;
		    index_ref := !G.aff_output_count;
		    G.aff_output_list :=  a :: !G.aff_output_list ;
	       );
	      G.IndexSet.add !index_ref list
	    in
	    let aff2outfix_func list aff_rule = 
	      let outfix_list = 
		close_func aff_rule None "" None level path []
	      in
	      List.fold_left (outfix_record_func) list outfix_list
	    in
	    (* convert continuations into outfix indexes *)
	    let index_list = 
	      List.fold_left (aff2outfix_func) G.IndexSet.empty continuations 
	    in
	    (* Utils.carp 4 "%d continuations, %d indexes\n" 
	      (List.length continuations) (G.IndexSet.cardinal index_list); *)
	    (* Utils.carp 3 "register new continuations in flag hash table\n" ; *)
	    let cont = 
	      get_cont index_list
	    in
	    (* Hashtbl.add G.flag_tbl 
	      continuations 
	      cont; *)
	    cont
	   )
	 )
      in
      rule.G.output := G.Processed(aff_list, cont);
      aff_list, cont
     )

(* end of function precompile_out *)

let rec close
    rule
    form
    tag
    fs
    level
    path
    aff_output_list
    = 
  
  let my_level = rule.G.rule_level in
  let my_no_stem = rule.G.rule_no_stem in
  let my_affix = rule.G.affix in
  let my_pass = rule.G.pass in
  let my_tag = rule.G.tag in
  let my_fs = rule.G.fs in
  let my_output = rule.G.output in

  try 
    let newform = 
      match form with 
	(* when calculated on level initials *)
      | None ->  
	  (* Utils.carp 5 "no form passed\n" ; *)
	  my_affix
      | Some(form) ->
	  (* Utils.carp 5 "form passed\n" ;*)
	  let newform = 
	    try 	  
	      let combine_pat = combine_func_make form in
	      combine_pat my_affix 
	    with Zero ->
	      my_affix
	  in
	  if my_pass then newform else my_affix
    in
    

    (* self with continuations as registered on flag_str *)
    (* recursion: is this correct???with [] *)
    let nth = List.length (List.find_all ((=) rule.G.morph_name) path) in
    assert ( Utils.lazy_carp 5 
      (lazy (Printf.eprintf "Recursion level %d/%d\n" nth rule.G.rule_recursion)); true );
    if nth >= rule.G.rule_recursion then raise No_match;

    let my_path = rule.G.morph_name :: path in
    Utils.lazy_carp 5 (
    lazy (
    let path_str = String.concat " " (my_path) in
    Printf.eprintf "%s\n" path_str
     ));

    let combine_outfix_func this_combine_func this_tag this_fs l = function 
	G.Outfix ( a, indref, freq_ref ) ->
	  try 
	    match a with G.Aff_output (affix,tag,fs,free,circ,cont) ->
	      let newaffix = this_combine_func affix in
	      let newtag = Tag.join this_tag tag in
	      let newfs = Fs.join this_fs fs in
	      let newaff_output = 
		G.Aff_output (newaffix,newtag,newfs,free,circ,cont)
	      in
	      let _, outfix =  get_aff_output false newaff_output in
	      (* new outfix is added to the list *)
		  outfix :: l
		 
	  with No_match -> l    
	      (* if there is no match, there no outfix is added *)
              
    in

    (* if this will be output then lemma_present flag will be set (how??)
       and realtime layer will take care of cutting the affix 
       if this will be merged (later) with a *)
    let my_affix, my_tag, my_fs, make_outfix_func = 
 
      if my_pass 
      then (
	(* let newtag = Tag.join tag my_tag in *)
	let newtag =  
	  match my_no_stem, newform, my_affix with 
	  | true, G.Lexis w, _ -> w ^ !G.lemma_delim_string 
	  | true, _, _ -> ""
		(* if combined with a cut=true after it'll be nulled (see line above) anyway *)
		(* so this is at times rather vacuous calculation *)
	  | _, _, G.Prefix(_,_,_) -> Tag.join my_tag tag
		(* this is a dirty hack *)
	  | _ -> Tag.join tag my_tag 
	in
	let newfs = Fs.join fs my_fs in
	let make_outfix_func list aff_rule = 
	  if aff_rule.G.pass then 
	    let outfix_list = 
	      close aff_rule (Some newform) newtag newfs my_level my_path list
	    in
	    outfix_list 
	  else (
	    let zero, combine_func = 
	      try false, combine_func_make newform 
	      with Zero -> true, fun x -> x
	    in
	    let zero = zero && my_tag = "" && my_fs = None in
	    if zero then 
	      let outfix_list = 
		close aff_rule (Some newform) newtag newfs my_level my_path list
	      in
	      outfix_list 
	    else 
	      let outfix_list = 
		close aff_rule (Some newform) newtag newfs my_level my_path []
	      in
	      let combine_outfix_func = combine_outfix_func (combine_func) newtag newfs in
	      List.fold_left (combine_outfix_func) list outfix_list
	   )
	in
	newform, newtag, newfs, make_outfix_func
       ) else (
	let zero, combine_func = 
	  try  false, combine_func_make my_affix  
	  with Zero -> true, fun x -> x
	in
	let zero = zero && my_tag = "" && my_fs = None in
	let make_outfix_func list aff_rule = 
	  if zero || aff_rule.G.pass then 
	    let outfix_list = 
	      close aff_rule (Some newform) my_tag my_fs my_level my_path list
	    in
	    outfix_list 
	  else (
	    let outfix_list = 
	      close aff_rule (Some newform) my_tag my_fs my_level my_path []
	    in
	    let combine_outfix_func = combine_outfix_func (combine_func) my_tag my_fs in
	    List.fold_left (combine_outfix_func) list outfix_list
	   )
	in
	my_affix, my_tag, my_fs, make_outfix_func
       )
    in
      
    let aff_list, cont = 
      precompile_out rule my_path close
    in

    let outfix_list = 
      (* Utils.carp 4 "outfixing %s\n" my_tag; *)
      if  my_pass || (rule.G.final = []) then (
	let outfix_list = 
	  List.fold_left (make_outfix_func) [] aff_list
	in
	let outfix_list = 
	  if rule.G.free || cont <> global_no_continuation 
	  then (
	    let free = rule.G.free in
	    (* Utils.carp 4 "adding final morph %s\n" my_tag; *)
	    let aff_output = 
	      G.Aff_output
		(my_affix,my_tag,my_fs,ref free,ref false, ref cont)
	    in
	    let _, outfix =  get_aff_output false aff_output in
	    outfix :: outfix_list
	   ) else 
	    outfix_list
	in
        rule.G.final <- outfix_list;
	outfix_list
       ) else 
	rule.G.final 
    in
    (* Utils.carp 4 "lists %d + %d\n" (List.length outfix_list) (List.length aff_output_list) ; *)
    List.rev_append outfix_list aff_output_list

  with No_match -> 
    
    aff_output_list 

