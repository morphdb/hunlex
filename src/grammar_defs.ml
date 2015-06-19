exception Bad_rule

module IndexSet = Set.Make( struct type t = int let compare = compare end )

type tag = string
type bv = Bv.t

type pattern = (string * Str.regexp) option
      
type fs = Kr.t option

type affix = 
  | Dummy of affix
  | Subst of string * bool * pattern 
  | Prefix of string * int * pattern
  | Suffix of string * int * pattern
  | Circumfix of affix * affix
  | Lexis of string
	
let grammar_debug = ref false
let global_recursion_limit = 4
let min_level = ref 0
let max_level = ref 0
let phono_cnt = ref 0
let morph_cnt = ref 0
let tag_cnt = ref 0
let usage_cnt = ref 0
let rule_cnt = ref 0
let lex_cnt = ref 0
let lexicon = ref false
let max_rule_cnt = ref 1000

let usage_bv = ref None
let features_bv = ref None

let use_fs = ref false


type steminfo = Tag | Lemma | Stem | LemmaWithTag | StemWithTag | NoSteminfo 

let tag_delim_string = ref ""
let lemma_delim_string = ref ""
let steminfo = ref LemmaWithTag

let process_tag lexicon lemma stem tag = 
  let stem = match stem with None -> "" | Some stem -> stem in
  if lexicon 
  then 
    match !steminfo with
    | Tag -> tag
    | Lemma ->  lemma
    | LemmaWithTag -> lemma ^ !lemma_delim_string ^ tag
    | StemWithTag  -> stem ^ !lemma_delim_string ^ tag
    | Stem -> stem
    | NoSteminfo -> ""
  else
    match !steminfo with
    | Tag | LemmaWithTag | StemWithTag -> tag
    | Lemma | Stem | NoSteminfo -> ""
	  

type morph = { 
    name : string ; 
    (* the level of a morpheme determines its combination 
			and precompilaction into affix-clusters of dictionary inclusion
     *)
    id : int ;
    mutable init : bool ;
    (* level is non-mutable since is set as a compile time option 
       so reading morph.conf sets it when the morph is created 
     *)
    level : int ;
    (* No stem is a boolean flag signaling annotation behaviour of the morph *)
    no_stem : bool ;
    (* I think recursion should be a compile time option as well?? *)
    mutable recursion_limit : int ;
    (* the most important member: the list of rules 
       governing the expression of the morphosyntactic category *)
    mutable rules : rule list }
and
cont = int * IndexSet.t * int ref * string ref

and
aff_output_t = Aff_output of affix * tag * fs * bool ref * bool ref * cont ref 

(* type 'a out_t = ((('a prerule) premorph) * Bv.t) list*)

and output_t = Processed of (rule list) * cont
(*  | Unprocessed of ( (output_t out_t) -> bv -> (output_t out_t) ) *
	( (output_t out_t) -> bv -> (output_t out_t) ) *
	bv*)
| Unprocessed of int * ( (morph * bool) list ) * bv

and
outfix_t = Outfix of aff_output_t * (int ref) * (int ref)

and

rule = { 
    rule_id : int ;
(*    morph : morph; *)
    rule_level : int;
    rule_recursion : int;
    rule_no_stem : bool;
    morph_name : string;

    free : bool;
    pass : bool;
    tag : tag;
    fs : fs ;
    usage : bv ;
    affix : affix;
    conditions : bv;
    neg_conditions : bv;
    keep : bv option ;

    mutable finalized : bool ;
    mutable output : output_t ref;
    mutable final :  outfix_t list;
    (* mutable clones : ((bv , rule) Hashtbl.t) option *)
  }


let make_morph name id level no_stem = { 
  name = name;
  init = false;
  id = id;
  level = level ;
  no_stem = no_stem ;
  recursion_limit = global_recursion_limit ;
  rules = [] 
}

let make_lemma name =
  let lemma = make_morph name !morph_cnt (!min_level) false in
  incr morph_cnt;
  lemma

let get_all_false_bv () =
  let bv = 
    match !features_bv with 
    | None -> 
	let bv = Bv.make !phono_cnt false in 
	features_bv := Some(bv); 
	bv;
    | Some(bv) -> bv;
  in
  bv


let get_all_false_usage_bv () =
  let bv = 
    match !usage_bv with 
    | None -> 
	let bv = Bv.make !usage_cnt false in 
	usage_bv := Some(bv); 
	bv;
    | Some(bv) -> bv;
  in
  bv

type out = morph list

(*type morph = rule premorph*)
type desc = {
    mutable _bv : Bv.t;
    mutable _bv_change : bool;
    mutable _out : (morph * bool) list ;
    mutable _cond : Bv.t;
    mutable _cond_change : bool;
    mutable _neg_cond: Bv.t;
    mutable _neg_cond_change : bool;
    mutable _lexis: bool;
    mutable _prefix: string option;
    mutable _prefix_clip: int;
    mutable _prefix_match: pattern;
    mutable _suffix: string option;
    mutable _subst: bool;
    mutable _suffix_clip: int;
    mutable _suffix_match: pattern;
    mutable _forall: bool;
    mutable _tag : tag;
    mutable _fs : fs;
    mutable _free : bool;
    mutable _pass : bool;
    mutable _usage: Bv.t;
    mutable _usage_change: bool;
    mutable _keep: Bv.t;
    mutable _keep_change: bool;
    mutable _filter: int list;
    mutable _recursion: int;
    mutable _level: int;
    mutable _no_stem : bool;
  }


type outfunc = desc -> desc

let ( tag_index : (string * int) list ref ) =  ref []
let ( morph_index : (string , morph) Hashtbl.t ) =  (Hashtbl.create 10001)
let ( phono_index : (string , int) Hashtbl.t  ) =  (Hashtbl.create 10001)
let ( usage_index : (string , (int * string)) Hashtbl.t  ) =  (Hashtbl.create 10001)
let usage_tags = ref ([] : string list)
let ( pat_index : (string , string) Hashtbl.t  ) = (Hashtbl.create 10001)
let ( abbr_index : (string , outfunc) Hashtbl.t  ) = (Hashtbl.create 10001)
let ( output_tbl : 
	( (int * (( morph * bool) list) * bv ),
	  (output_t ref)) Hashtbl.t  ) = 
  Hashtbl.create 10001

let ( cont_tbl : ( IndexSet.t, cont ) Hashtbl.t )  = Hashtbl.create 10001
let ( clone_tbl : ( (int*bv), rule ) Hashtbl.t )  = Hashtbl.create 10001
let index_set_count = ref 0

module Aff = struct
  type t = aff_output_t 
  let equal 
      ( Aff_output (a,t,fs,free,circ,cont) )
      ( Aff_output (a',t',fs',free',circ',cont') )
      = 
    a = a' && t = t' && fs = fs' && free = free' && circ = circ' && !cont == !cont'
  let hash ( Aff_output ( a,t,fs,_,_,cont) ) = 
    let i, _, _, _ = !cont in
    Hashtbl.hash ( a, t, fs, i )
  (* Hashtbl.hash ( a, t, fs ) *)
end
    
module Aff_tbl = Hashtbl.Make(Aff)

let ( aff_tbl : 
	outfix_t Aff_tbl.t  ) = 
  Aff_tbl.create 100001


module Dic = struct
  type t = aff_output_t 
  let equal 
      ( Aff_output (a,t,fs,_,_,_) )
      ( Aff_output (a',t',fs',_,_,_) )
      = 
    a = a' && t = t' && fs = fs' 
  let hash ( Aff_output ( a,t,fs,_,_,_) ) = Hashtbl.hash ( a, t, fs )
end
    
module Dic_tbl = Hashtbl.Make(Dic)

let ( dic_tbl : 
	(bool ref * cont ref ) Dic_tbl.t ) = 
  Dic_tbl.create 100001

let ( compound_flag_tbl : (string, string ref) Hashtbl.t) = (Hashtbl.create 1001)
let ( compound_patterns : (string ref * string ref) list ref) = ref []

let guess_flag = ref ""

(*let ( flag_tbl : 
	( (rule list),
	  cont ) Hashtbl.t ) = 
  Hashtbl.create 10001
*)
let ( pref_flag_tbl : 
	( int,
	  (int list ref * string ref) ) Hashtbl.t ) = 
  Hashtbl.create 10001


let dic_output_count = ref 0
let aff_output_list = ref ([] : aff_output_t list)
let aff_output_count = ref 0

let rules = ref ([] : rule list)

let compound_flags () = ref []

let default_desc () = {
  _bv = get_all_false_bv();
  _bv_change = false;
  _out = [];
  _cond = get_all_false_bv();
  _cond_change = false;
  _neg_cond = get_all_false_bv();
  _neg_cond_change = false;
  _lexis = false;
  _prefix = None;
  _prefix_clip = 0;
  _prefix_match = None;
  _suffix = None;
  _subst = false;
  _suffix_clip = 0;
  _suffix_match = None;
  _forall = false;
  _tag = "";
  _fs = None;
  _free = not !lexicon ; (* this is a rather debatable default *)
  _pass = false ; (* only filters will pass *)
  _usage = get_all_false_usage_bv();
  _usage_change = false;
  _keep = get_all_false_bv();
  _keep_change = false;
  _filter = [];
  _recursion = 0;
  _level = 0;
  _no_stem = false;
}

let make_desc ?(desc=default_desc()) () = 
  {
   _bv = desc._bv;
   _bv_change = false;
   _out = desc._out;
   _cond = desc._cond;
   _cond_change = false;
   _neg_cond = desc._neg_cond;
   _neg_cond_change = false;
   _lexis = desc._lexis;
   _prefix = desc._prefix;
   _prefix_clip = desc._prefix_clip;
   _prefix_match = desc._prefix_match;
   _subst = desc._subst;
   _suffix = desc._suffix;
   _suffix_clip = desc._suffix_clip;
   _suffix_match = desc._suffix_match;
   _forall = false;
   _tag = desc._tag;
   _fs = desc._fs;
   _free = desc._free;
   _pass = desc._pass;
   _usage = desc._usage;
   _usage_change = false;
   _keep = desc._keep;
   _keep_change = false;
   _filter = desc._filter;
   _level = desc._level;
   _recursion = desc._recursion;
   _no_stem = desc._no_stem;
 }

let add_rule (morph : morph) desc id rules = 
  
  let fs = desc._fs in
  let usage = desc._usage in
  let bv = desc._bv in
  let cond = desc._cond in
  let neg_cond = desc._neg_cond in
  let free = desc._free in
  let pass = desc._pass in
  let recursion = desc._recursion in
  let level = desc._level in
  let no_stem = desc._no_stem in
  let morph_name = morph.name in

  let affix = 

    match desc._subst, desc._lexis, desc._prefix, desc._suffix with 
    | false, false, None, None when !lexicon 
      -> Lexis(morph.name) (* this does not add the rule by itself that has to be created *)
    | false, false, None, None when not !lexicon
      -> Suffix("",desc._suffix_clip,desc._suffix_match) (* zero morpheme & truncation in grammar *)
    | false, true , None, Some l 
      -> Lexis(l)
    | true, true, None, Some l  
      -> Dummy(Lexis(l))  (* should ckeck for matches empty matches for all lexes *)
    | false, false, None, Some s
      -> Suffix(s,desc._suffix_clip,desc._suffix_match) (* this includes zero morpheme & truncation in grammar *)
    | false, false, Some p, None
      -> Prefix(p,desc._prefix_clip,desc._prefix_match)
    | false, false, Some p, Some s 
      -> Circumfix(
	Prefix(p,desc._prefix_clip,desc._prefix_match),
	Suffix(s,desc._suffix_clip,desc._suffix_match)
       )
    | true, false, None, Some s 
      -> Subst(s, desc._forall, desc._suffix_match)
    | true, false, None, None
      -> Subst("", desc._forall, desc._suffix_match)
    | _, _, _, _ 
      -> Utils.carp 0 
	  "this specification cannot be mapped on a rule (e.g., lexis and substitution exclude each other and an affix)\n";
	raise Bad_rule
  in
  let keep = 
    if Bv.all_zeros desc._keep 
    then None 
    else Some(desc._keep) in

  let level = morph.level in

  (* let out_key = match desc._out with Some(f) -> f [] bv | None -> [] in *)
  let out_key = desc._out in
  
  let out_value = 
    if keep = None then
      try 
 	let v = Hashtbl.find output_tbl (level, out_key, bv) in
	(* Utils.carp 5 "precedented\n"; *)
	v      
      with Not_found -> 
	let new_out_value = ref ( Unprocessed (level, out_key, bv )) in
	Hashtbl.add output_tbl (level, out_key, bv) new_out_value;
	(* Utils.carp 4 "unprecedented\n"; *)
	new_out_value;
    else
      ref ( Unprocessed (level, out_key, bv ))
  in

  let tag = process_tag !lexicon morph.name desc._suffix desc._tag in
  let tag = 
	if !grammar_debug 
	then tag ^ "[" ^ morph.name ^ "/" ^ (string_of_int id) ^ "]" 
	else tag 
  in 

  let free = !grammar_debug || free in

  let rule = 
    { 
      rule_id = id ;
      (* morph = morph; *)
      rule_level = level;
      rule_recursion = recursion;
      rule_no_stem = no_stem;
      morph_name = morph_name;
      affix = affix;
      tag = tag;
      fs = fs ;
      free = free;
      pass = pass;
      usage = usage; 
      conditions = cond;
      neg_conditions = neg_cond;

      output = out_value ;
      keep = keep;
      final = [];
      finalized = false;
      (* clones = clones; *)
    }
  in
  morph.rules <- rule :: morph.rules;
  rule :: rules

let make_clone rule pos neg = 
  let process out_value = match !out_value with 
    Unprocessed(l,out_key,bv) ->
      (* let apply (morph,bv) = 
	 let bv = Bv.bw_or bv pos in
	 (morph,bv)
	 in
      
	 let out_key = List.rev_map (apply) out_key in
       *)
      let bv = Bv.bw_or bv pos in
      let out_value = 
	(* this should be left uncommented!!! *)
	try 
	  let v = Hashtbl.find output_tbl (l, out_key, bv) in
	  (* Utils.carp 5 "precedented\n"; *)
	  v      
	with Not_found -> 
	  let new_out_value = ref ( Unprocessed (l, out_key, bv )) in
	  Hashtbl.add output_tbl (l, out_key, bv) new_out_value;
	  (* Utils.carp 4 "unprecedented\n"; *)
	  new_out_value
      in
      out_value
  | _ -> Utils.carp 0 "impossible\n"; raise (Failure "oops")
  in
  let rule_id = !rule_cnt in
  incr rule_cnt;
  let rule = 
    { 
      rule_id = rule_id ;
      rule_level = rule.rule_level;
      rule_recursion = rule.rule_recursion;
      rule_no_stem = rule.rule_no_stem;
      morph_name = rule.morph_name;
      (* morph = rule.morph ; *)
      affix = rule.affix ;
      tag = rule.tag;
      fs = rule.fs ;
      free = rule.free;
      pass = rule.pass;
      usage = rule.usage; 
      conditions = Bv.bw_or pos rule.conditions;
      neg_conditions = Bv.bw_or neg rule.neg_conditions;
      output = process rule.output ;
      final = [];
      keep = None;
      finalized = false;
      (* clones = None; *)
    }
  in
  rule


  
(* we could make some error checking here, but let us just
   have silent overwrite policy *)

let set_lexis lexis desc = 
  desc._lexis <- true; 
  desc._suffix <- Some lexis; 
  desc

let set_prefix prefix desc = 
  desc._prefix <- Some prefix; 
  desc
      
let set_suffix suffix desc = 
  desc._suffix <- Some suffix; 
  desc

let set_subst subst desc = 
  desc._subst <- true ; 
  desc._suffix <- Some subst; 
  desc


let set_zero desc =
  desc._suffix <- Some ""; 
  desc

let set_circumfix (prefix,suffix) desc = 
  desc._prefix <- Some prefix; 
  desc._suffix <- Some suffix; 
  desc

let set_fs fs desc = 
  desc._fs <- fs;
  desc

let join_fs fs desc = 
  desc._fs <- Fs.join desc._fs fs;
  desc

let set_phono bool i desc  = 
  let bv = 
    if desc._bv_change 
    then desc._bv 
    else (
      let bv = Bv.copy desc._bv in
      desc._bv <- bv;
      desc._bv_change <- true;
      bv
     )
  in
  Bv.set bv i bool;
  desc

let set_phono_cond i desc  = 
  let cond = 
    if desc._cond_change
    then desc._cond
    else (
      let cond = Bv.copy desc._cond in
      desc._cond <- cond;
      desc._cond_change <- true;
      cond
     )
  in
  Bv.set cond i true;
  desc

let set_neg_phono_cond bool i desc  = 
  let cond = 
    if desc._neg_cond_change
    then desc._neg_cond
    else (
      let cond = Bv.copy desc._neg_cond in
      desc._neg_cond <- cond;
      desc._neg_cond_change <- true;
      cond
     )
  in
  Bv.set cond i bool;
  desc

let set_free bool desc = 
  desc._free <- bool;
  desc

let set_pass bool desc = 
  desc._pass <- bool;
  desc

let add_morph morph desc = 
  desc._out <- 
    if not (List.mem_assq morph desc._out) then (morph, true) :: desc._out else desc._out;
  desc
    
let remove_morph morph desc = 
  desc._out <- 
    List.remove_assq morph  desc._out;
  desc

let set_tag (_,tag) desc =
  desc._tag <- tag;
  desc

let add_tag (_,tag) desc =
  desc._tag <- desc._tag (* !Output.tag_delim_string *) ^ tag;
  desc

let set_clip clip desc = 
  desc._prefix_clip <- clip;
  desc._suffix_clip <- clip;
  desc

let set_clip_ini clip desc = 
  desc._prefix_clip <- clip;
  desc

let set_clip_fin clip desc = 
  desc._suffix_clip <- clip;
  desc

let set_match pattern desc = 
  desc._prefix_match <- pattern;
  desc._suffix_match <- pattern;
  desc

let set_match_forall pattern desc = 
  desc._forall <- true;
  desc._subst <- true;
  desc._suffix_match <- pattern;
  desc


let set_match_ini pattern desc = 
  desc._prefix_match <- pattern;
  desc

let set_match_fin pattern desc = 
  desc._suffix_match <- pattern;
  desc

let set_filter filter desc = 
  (* desc._pass <- true; *)
  desc._filter <- filter :: desc._filter ;
  desc

let set_recursion recursion desc = 
  (* desc._pass <- true; *)
  desc._recursion <- recursion ;
  desc

let set_keep bool i desc  = 
  let keep = 
    if desc._keep_change
    then desc._keep
    else (
      let keep = Bv.copy desc._keep in
      desc._keep <- keep;
      desc._keep_change <- true;
      keep
     )
  in
  Bv.set keep i bool;
  desc

let set_usage bool (i,tag) desc  = 
  let usage = 
    if desc._usage_change
    then desc._usage
    else (
      let usage = Bv.copy desc._usage in
      desc._usage <- usage;
      desc._usage_change <- true;
      usage
     )
  in
  Bv.set usage i bool;
  let desc = 
    try 
      let tag_index = List.assoc "USAGE" !tag_index in 
      add_tag (tag_index, tag) desc
    with Not_found -> 
      desc
  in
  desc

let make_compound_morph x = 
  let compound_morph = make_morph x 0 (!max_level) false in
  let desc = make_desc () in 
  let desc  = set_lexis x desc in
  let desc = set_free true desc in
  desc._recursion <- compound_morph.recursion_limit;
  desc._subst <- true;
  Utils.lazy_carp 4 (lazy (Printf.eprintf "Added a dummy rule for %s\n" x));
  let id = !rule_cnt  in
  incr rule_cnt;
  rules := add_rule compound_morph desc id !rules;
  compound_morph
