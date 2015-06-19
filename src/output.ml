(* uses Grammar_defs Printf *)

exception Unable_to_output
exception Not_enough_flags

module G = Grammar_defs
type mode = Spellchecker | Stemmer | Analyzer | NoMode

let mode = ref NoMode
let output_pos = ref 0

let fs_debug = ref false
let out_delim_string = ref ""
let out_delim_dic_string = ref ""

(* let flag_delim_string = ref "" *)
(* available flags *)
let flags = ref ""
(* double flags *)
let double_flags = ref false

let comp_cnt = ref 0
let flag_cnt = ref 0

let get_single_flag () = 
  let x = !comp_cnt in 
  if x >= !flag_cnt
  then (
    Utils.carp 0 
      "Not enough flags: consider using double flags\n"; 
    raise Not_enough_flags;
   );
  incr comp_cnt;
  String.sub !flags x 1


let get_double_flag () = 
  let x = !comp_cnt / !flag_cnt in
  if x >= !flag_cnt
  then (
    Utils.carp 0 
      "Not enough flags: consider using a custom flag file with double flags\n"; 
    raise Not_enough_flags;
   );
  let y = !comp_cnt mod !flag_cnt in
  incr comp_cnt;
  Printf.sprintf "%c%c" (String.get !flags x) (String.get !flags y)
			
let get_flag = ref get_single_flag

let control_strings = 
  [ "PSEUDOROOT";
    "LEMMA_PRESENT";
    "CIRCUMFIX";
(*    "COMPOUNDBEGIN";
    "COMPOUNDMIDDLE";
    "COMPOUNDEND";
    "ONLYINCOMPOUND";
    "FORBIDDENWORD";*)
  ]
    
let pseudoroot_flag = ref ""
let lemma_present_flag = ref ""
let circumfix_flag = ref ""

let controls = List.rev_map (fun x -> (x, ref "")) control_strings
    
let init_affix affix_channel = 

  (* meta information *)

  let set (_, flag_ref) = flag_ref := !get_flag () in
  List.iter (set) controls;
  
  Utils.carp 1 "%d control flags defined\n" !comp_cnt;

  let os_type = Sys.os_type in
  let ocaml_version = Sys.ocaml_version in

  let _where = os_type in
  let _who = "Hunlex v0.3 (w OCaml " ^ ocaml_version ^ ")" in
  let _when = if os_type = "Unix" then (
    let tm = Unix.localtime (Unix.time ()) in
    Printf.sprintf "%d:%d:%d on %d/%d/%d"
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year)
   ) else ""
  in
  let _how_long = Sys.time () in 
  
  let _ = 
    Printf.fprintf affix_channel 
      "# Automatically generated\n#  by %s\n#  on %s\n#  at %s\n#  in %f seconds\n\n"
      _who _where _when _how_long
  in
  output_pos := pos_out affix_channel;
  Printf.fprintf affix_channel "                        \n";
  let dump_compound_pattern ( mod_flag,  cat_flag ) = 
    Printf.fprintf affix_channel "              \n" 
  in
  List.iter (dump_compound_pattern) !G.compound_patterns;
  Printf.fprintf affix_channel "                \n" (* guess flag *);

  let dump_setting y (x, flag_ref) = y ^ "\n" ^ x ^ " " ^ !flag_ref in
  Printf.fprintf affix_channel "%s\n%s\n%s\n" 
    (if !double_flags then "FLAG long\n" else "")
    "SET ISO8859-2\n"
    (List.fold_left (dump_setting) "" controls);

  pseudoroot_flag := !(List.assoc "PSEUDOROOT"  controls);
  lemma_present_flag := !(List.assoc "LEMMA_PRESENT"  controls);
  circumfix_flag := !(List.assoc "CIRCUMFIX"  controls);
  ()


let rec pretty_print_affix flag aff flag_str tag = 
  (* Utils.carp 4 "Dumping affix\n"; *)
      let rec app a = 
	match a with   
    | G.Prefix(app,clipint,pattern) ->
	true, app, clipint, pattern 
    | G.Suffix(app,clipint,pattern) ->
	false, app, clipint, pattern 
    | G.Dummy(s) -> 
	(* app s *)
	raise (Failure "Oops")
    | G.Circumfix(p,s) -> Utils.carp 0 "circumfix cannot be output\n";
	raise Unable_to_output
    | G.Lexis(w) -> Utils.carp 0 "lexis '%s' cannot be output\n" w;
	raise Unable_to_output
    | G.Subst(_,_,_) -> Utils.carp 0 "subst cannot be output\n";
	raise Unable_to_output
  in
  let pref, app, clipint, pattern = app aff in
  let pattern_str = 
    match pattern with 
    | None -> "."
    | Some(pattern_str,_) -> 
	let len = String.length pattern_str in
	let cut = 
	  if pref then 
	    pattern_str.[0] = '^'
	  else 
	    pattern_str.[len-1] = '$'
	in
	let pattern_str =
	  if cut then 
	    String.sub pattern_str (if pref then 1 else 0) (len - 1) 
	  else 
	    pattern_str
	in
	pattern_str
  in
  let clip = 
    if clipint = 0 
    then "0" 
    else (
      if pattern = None 
      then (
	Utils.carp 0 "For clipping you have to provide the characters to clip\n";
	raise Unable_to_output
       ) else (
	let len = String.length pattern_str in
	let clip = 
	  String.sub pattern_str (if pref then 0 else len - clipint) clipint 
	in
	if (String.contains clip ']') || (String.contains clip '[')
	then (
	  Utils.carp 0 "Illegal clip/match combination '%s' '%s'\n" pattern_str clip;
	  raise Unable_to_output;
	 );
	clip
       )
     )
  in
  let app = if app = "" then "0" else app in 
  let app = if flag_str = "" then app else app ^ "/" ^ flag_str in
  let aff_type = if pref then "PFX" else "SFX" in
  String.concat !out_delim_string
    [aff_type; flag; clip; app; pattern_str; tag]

	
let log_lexis aff = 
  match aff with G.Lexis(w) -> w 
  | _ -> Utils.carp 0 "not a lexis\n"; 
      raise (Invalid_argument "log_lexis")
	
let pretty_print_final flag f = 
  match f with G.Aff_output(aff,tag,_,free,circ,cont) ->
    let _,_,_,flag_str = !cont in
    let flags = !flag_str in
    let flags = if !free then flags else flags ^ !pseudoroot_flag in
    let flags = if !circ then flags ^ !circumfix_flag else flags in
    pretty_print_affix flag aff flags tag

let pretty_print_lexis = function 
    G.Aff_output(G.Lexis(base),tag,fs,free,_,cont) ->
      let _,_,_,flag_str = !cont in
	let tag, lp = 
	  let rex = Str.regexp_string base in
	  if Str.string_match rex tag 0 then (
	    let baselen = String.length base in
	    try 
	      String.sub tag baselen (String.length tag - baselen)
		, ""
	    with 
	      x -> Utils.carp 0 "tag %s, base %s\n" tag base;
		raise x
	   ) else
	    tag, !lemma_present_flag
	in
	
	let rof = if !free then "" else !pseudoroot_flag in 
	let fs_string = if !fs_debug then !out_delim_dic_string ^ (Fs.print fs) else "" in
	let flags = !flag_str in
	let flags = flags ^ rof ^ lp in
	let flags = if flags = "" then "" else "/" ^ flags in
	base ^ flags ^ !out_delim_dic_string ^ tag ^ fs_string ^ "\n";
  | _ -> raise (Invalid_argument "pretty_print_lexis")

 


