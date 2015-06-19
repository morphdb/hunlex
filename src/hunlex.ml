(* uses Close Output Grammar_defs Utils Flag
   Grammar_parser Morph_parser Phono_parser *)

module G = Grammar_defs

exception Parse_error of string

let variant_cnt = ref 0
   
let message = Utils.carp 1

let read_grammar 
    morph_buffer phono_buffer usage_buffer annot_buffer grammar_buffer 
    =
  
  message "Reading morpheme declarations and levels\n";
  flush stderr;
  let _ = try Morph_parser.main Morph_lexer.token morph_buffer
  with Parsing.Parse_error 
    -> Utils.carp 0 "Error parsing morpheme declaration file\n"; 
      raise ( Parse_error "morpheme declaration file" )
  in
  (* message "ok\n"; *)
  flush stderr;

  message "Reading phono features\n";
  flush stderr;
  let _ = try Phono_parser.main Phono_lexer.token phono_buffer 
  with Parsing.Parse_error 
    -> Utils.carp 0 "Error parsing phono features file\n"; 
      raise ( Parse_error "phono declaration file" )
  in
  (* message "ok\n"; *)
  flush stderr;
  
  message "Reading usage qualifiers\n";
  flush stderr;
  let _ = try Usage_parser.main Usage_lexer.token usage_buffer 
  with Parsing.Parse_error 
    -> Utils.carp 0 "Error parsing usage qualifiers file\n";
      raise ( Parse_error "usage qualifiers file" )
  in
  (* message "ok\n"; *)
  flush stderr;

  message "Reading annotation configuration\n";
  flush stderr;
  let _ = try Annot_parser.main Annot_lexer.token annot_buffer 
  with Parsing.Parse_error 
    -> Utils.carp 0 "Error parsing annotation configuration file\n";
      raise ( Parse_error "annotation configuration file" )
  in
  (* message "ok\n"; *)
  flush stderr;

  message "\nParsing the grammar\n";
  flush stderr;
  let rules, rule_cnt  = 
    try Grammar_parser.main Grammar_lexer.token  grammar_buffer
    with Parsing.Parse_error 
      -> Utils.carp 0 "Error parsing grammar file\n"; 
	raise ( Parse_error "grammar file" )
  in
  (* message "ok\n";*)

  G.rules := rules;
  G.rule_cnt := rule_cnt;
  Utils.carp 1 "%d initial grammar rules\n" !G.rule_cnt;
  flush stderr;
  rules, rule_cnt

let read_lexicon lexicon_buffer close = 
  message "\nParsing the lexicon and performing closure on levels (this may take some time)\n";
  flush stderr;
  
  (* let lexicon = Lexicon.make() in *)
  G.morph_cnt := 0;
  variant_cnt := 0;
  G.lexicon := true;
  let close_func rule = 
    let count = List.length (Close.close rule None "" None 0 [] []) in
    variant_cnt := !variant_cnt + count 
  in
  let _ = try
    while true 
    do 
      let entry = 
	Grammar_parser.entry Grammar_lexer.token lexicon_buffer
      in
      match entry with None -> () | Some(entry) ->
	assert (Utils.lazy_carp 4 (lazy(Printf.eprintf "%d\t%s\n" !G.morph_cnt entry.G.name)); true);
	flush stderr;

	if close then 
	  (* close entry tag fs features level path outfix_list *)
	  List.iter (close_func) entry.G.rules;
    done
  with 
  | End_of_file -> () (* Utils.carp 0 "ok\n" *)
  | Parsing.Parse_error -> 
      Utils.carp 0 "Error parsing lexicon file\n";
      raise ( Parse_error "lexicon file" )
  in
  (* message "ok\n"; *)
  Utils.carp 1 "\n%d lexical entries\n" !G.morph_cnt;
  Utils.carp 1 "%d allomorphic variants\n" !variant_cnt;
  Utils.carp 1 "%d distinct dictionary entries\n" !G.dic_output_count;
  Utils.carp 1 "%d affix cluster output rules\n" !G.aff_output_count;
  flush stderr
  

let dump_resources 
    (* lexicon lex_cnt *) aff_channel dic_channel 
    = 

  message "Dynamically allocating flags\ndumping affix file\n";
  flush stderr;
  (* dump affix header *)
  Output.init_affix aff_channel;
  (* calculate flags and dump affixes *)
  Flag.create aff_channel;
  Utils.carp 1 "%d distinct paradigms\n" !G.index_set_count;
  Utils.carp 1 "%d flags used\n" !Output.comp_cnt;
  seek_out aff_channel !Output.output_pos;
  Printf.fprintf aff_channel "FLAG_COUNT %d\n" !Output.comp_cnt;
  let dump_compound_pattern (mod_flag,cat_flag) = 
    if !mod_flag <> "" && !cat_flag <> "" then 
	Printf.fprintf aff_channel "COMPOUND %s %s\n" !mod_flag !cat_flag
  in
  List.iter (dump_compound_pattern) !G.compound_patterns;
  if !G.guess_flag <> "" 
  then Printf.fprintf aff_channel "GUESS_FLAG %s\n" !G.guess_flag;
  close_out aff_channel;

  message "Dumping precompiled stems to dictionary file\n";
  flush stderr;
  Printf.fprintf dic_channel "%d\n" !G.dic_output_count;
  let pretty_print lexis _ = 
    Printf.fprintf dic_channel "%s" (Output.pretty_print_lexis lexis)
  in
  G.Dic_tbl.iter (pretty_print) G.dic_tbl;
  close_out dic_channel

let main 
    morph_buffer phono_buffer usage_buffer annot_buffer grammar_buffer lexicon_buffer 
    aff_channel dic_channel 
    =
  let rules, rule_cnt = 
    read_grammar 
      morph_buffer phono_buffer usage_buffer annot_buffer grammar_buffer  
  in
  let _ (* , lexicon *) = 
    read_lexicon
      lexicon_buffer true
  in
  dump_resources 
    (* lexicon  lex_cnt *) aff_channel dic_channel 
