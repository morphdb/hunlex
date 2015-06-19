(* uses Hunlex Output Flag Arg Lexing Grammar_defs *)

module G = Grammar_defs

let _ = 
  
  let version_string = "0.3" (* 14/04/2005 *) in
  Utils.carp 1 "Hunlex %s (c) TV\n" version_string;

  let usage  =
    "Usage: hunlex <options>\nOptions: (for more see manpage)\n   option\tdescription (default settings)\n------------------------------------------"
  in
 
  let help_ref = ref false in
  let version_ref = ref false in
 
  let others string = 
    Utils.carp 0 "Oops: options require leading '-'. Watch '%s'.\n" string;
    help_ref := true
  in
 
  let lexicon_file_name = ref "lexicon" in
  let grammar_file_name = ref "grammar" in
  let phono_file_name = ref "phono.conf" in
  let morph_file_name = ref "morph.conf" in
  let annot_file_name = ref "annot.conf" in
  let usage_file_name = ref "usage.conf" in
  let signature_file_name = ref None in

  let aff_file_name = ref "affix.aff" in
  let dic_file_name = ref "dictionary.dic" in

  let flags_file_name = ref None in

  G.min_level := 0;
  G.max_level := 1000;

  (* this should be an output settings "resource" object with constructor initialization.
     At present all is initialized to the empty string 
     and sensibly set here so that it can be seen 
   *)

  (*
     mode is nullable 
     Output.mode := Output.NoMode; modes are not implemented fully yet
     steminfo is by default none
     Output.steminfo := Output.Tag;
   *)

  G.tag_delim_string := "";
  G.lemma_delim_string := "";
  Output.out_delim_string := " ";
  Output.out_delim_dic_string := "\t";
  Output.double_flags := false;
  Utils.debug_level := 0;
  G.grammar_debug := false;
  G.use_fs := false;

  let synthesis = ref false in
  let synth_in_file_name = ref "-" in
  let synth_out_file_name = ref "-" in

  let speclist = [

    (* main operation options *)
    "-synthesis", Arg.Set synthesis, "\tmorphological synthesis (no)";

    (* input file options *)
    "-synth_in", Arg.String ((:=) synth_in_file_name), "\tsynthesize from file (stdin)";
    "-synth_out", Arg.String ((:=) synth_out_file_name), "\tsynthesize to file (stdout)";

    "-lexicon", Arg.String ((:=) lexicon_file_name), "\tlexicon (lexicon)";
    "-grammar", Arg.String ((:=) grammar_file_name), "\tmorphological grammar file (grammar)";
    "-phono", Arg.String ((:=) phono_file_name), "\tphono features file (phono.conf)";
    "-morph", Arg.String ((:=) morph_file_name), "\tmorph declarations and levels file (morph.conf)";
    "-annot", Arg.String ((:=) annot_file_name), "\ttag (annotation) configuration (annot.conf)";
    "-usage", Arg.String ((:=) usage_file_name), "\tusage qualifiers and their tags (usage.conf)";
    "-signature", Arg.String (fun x -> if x <> "" then signature_file_name := Some(x)), "\tfeature structure signature (None)";

    (* output file options *)

    "-aff", Arg.String ((:=) aff_file_name), "\toutput affix file (affix.aff)";
    "-dic", Arg.String ((:=) dic_file_name), "\toutput dictionary file (dictionary.dic)";

    (* output mode/info options *)

    "-mode", Arg.String 
      (fun x -> try Output.mode := match x with 
      | "Spellchecker" -> Output.Spellchecker 
      | "Stemmer"      -> Output.Stemmer
      | "Analyzer"     -> Output.Analyzer
      | "NoMode"       -> Output.NoMode
      | _ -> raise (Failure "")
      with Failure _ -> 
	Utils.carp 0 "Illegal argument of -mode. Has to be either of\n"; 
	Utils.carp 0 "Spellchecker|Stemmer|Analyzer|NoMode\n"
      ), "\toutput mode [Spellchecker|Stemmer|Analyzer|NoMode] (NoMode)";

    "-steminfo", Arg.String
      (fun x -> try G.steminfo := match x with 
      | "Tag"        -> G.Tag
      | "Lemma"      -> G.Lemma
      | "Stem"       -> G.Stem
      | "LemmaWithTag"      -> G.LemmaWithTag
      | "StemWithTag"       -> G.StemWithTag
      | "NoSteminfo" -> G.NoSteminfo
      | _ -> raise (Failure "")
      with Failure _ -> 
	Utils.carp 0 "Illegal argument of -steinfo. Has to be either of\n"; 
	Utils.carp 0 "Tag|Lemma|Stem|LemmaWithTag|StemWithTag|NoSteminfo] (NoSteminfo)\n"
      )
      , "\tinfo to output about a word's stem [Tag|Lemma|Stem|LemmaWithTag|StemWithTag|NoSteminfo] (NoSteminfo)";
    
    (* output formatting options *)

    (* delimiters *)

    "-lemma_delim", Arg.String ((:=) G.lemma_delim_string), "\tlemma delimiter (':')";
    "-tag_delim", Arg.String ((:=) G.tag_delim_string), "\ttag delimiter ('')";
    "-out_delim", Arg.String ((:=) Output.out_delim_string), "\toutput delimiter (<space>)";
    "-out_delim_dic", Arg.String ((:=) Output.out_delim_dic_string), "\toutput delimiter for dic file (<tab>)";

    (* flags *)
    "-double_flags", Arg.Set Output.double_flags, "\t [0-9][^0-9] type double-char flags (no, single char)";
    "-flags", Arg.String (fun x -> if x <> "" then flags_file_name := Some(x)), "\tlegitimate flag characters file (none, use predefined flags)";

    (* compile options *)

    "-min_level", Arg.Int ((:=) G.min_level), "\tminimum morph level (0)";
    "-max_level", Arg.Int ((:=) G.max_level), "\tmaximum morph level (1000)";

    (* debugging options *)

    "-debug_level", Arg.Int ((:=) Utils.debug_level), "\tdebug level (no)";
    "-fs_debug"
      , Arg.Set Output.fs_debug
      , "\toutput fs in the dictionary for testing purposes (no)";
    "-grammar_debug", Arg.Set G.grammar_debug, "\ttestable output (no)";

    "--version", Arg.Set version_ref, "\tDisplay version info and exit";
  ]
  in
  Arg.parse speclist others usage;
  if !help_ref then (
  	Arg.usage speclist usage;
	exit 0;
  );
  if !version_ref then ( print_string version_string; print_newline(); exit 0 );


  let make_buffer file_name file_desc = 
    try Lexing.from_channel 
	(if file_name = "-" then stdin else open_in file_name)
    with error -> 
      Utils.carp 0 "Error opening %s file '%s' for reading\n" 
	file_desc file_name; 
      raise error
  in

  let morph_buffer = make_buffer !morph_file_name "morpheme declaration" 
  and phono_buffer = make_buffer !phono_file_name "phono-feature declaration" 
  and usage_buffer = make_buffer !usage_file_name "usage qualifiers file"
  and annot_buffer   = make_buffer !annot_file_name "annotation configuration"

  and grammar_buffer = make_buffer !grammar_file_name "grammar file"
  and lexicon_buffer = make_buffer !lexicon_file_name "lexicon file"
  in

  let make_out_channel file_name file_desc = 
    if !file_name = "-" then stdout else (
    try open_out !file_name
    with error -> 
      Utils.carp 0 "Error opening %s file '%s' for writing\n" 
	file_desc !file_name; 
      raise error
   )
  in
  
  Output.flags := (
    match !flags_file_name with 
    | Some (flags_file_name) -> 
	let flags_in = 
	  try open_in flags_file_name 
	  with error -> Utils.carp 0 "Error opening flags file '%s' for reading\n" 
	      flags_file_name; 
	    raise error
	in
	let flags = 
	  try input_line flags_in 
	  with End_of_file -> Utils.carp 0 "Empty flags file\n";
	    raise (Failure "Empty flags file")
	in
	flags (* would need proper processing eliminating redundancies, etc.*)
    | None ->
	(* default flags *) 
	"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ¢£¥§©ª«¬®²³µ¶¹ºÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"
   );

  let flags_len = String.length !Output.flags in
  Output.flag_cnt := flags_len;
  if !Output.double_flags then (
    Utils.carp 1 "using long (double character) flags\n";
    Utils.carp 1 "%d available flags\n" (flags_len * flags_len) ;
    Output.get_flag := Output.get_double_flag;
   ) else (
    Utils.carp 1 "using short (single character) flags\n";
    Utils.carp 1 "%d available flags\n" flags_len;
    Output.get_flag := Output.get_single_flag;
   );
  

  
  (* main processing clause *)



  let _ = 
    match !signature_file_name with 
      Some(signature) -> 
	Utils.carp 1 "reading feature structure signature from '%s'...\n" signature;
	let signature_buffer = make_buffer signature "signature" in
	let _ = Fs.read_buffer signature_buffer in
	G.use_fs := true;
	Utils.carp 1 "ok\n"
    | None -> ()
  in

  let _ = 
    if !synthesis 
    then (
      (*
      let synthesis_buffer = make_buffer !synth_in_file_name "synthesis input" 
      and synth_out_channel = make_out_channel synth_out_file_name "synthesis output" 
      in
      Synthesis.synthesize 
	morph_buffer
	phono_buffer
	usage_buffer
	grammar_buffer
	lexicon_buffer
	
	synthesis_buffer
	synth_out_channel;
     
      
      close_out synth_out_channel;
       *)
     )
    else (
      let dic_channel = make_out_channel dic_file_name "dictionary"
      and aff_channel = make_out_channel aff_file_name "affix"
      in
      
      Hunlex.main 
	morph_buffer
	phono_buffer
	usage_buffer
	annot_buffer
	grammar_buffer
	lexicon_buffer
	
	aff_channel
	dic_channel;

     )
  in

  exit 0;

  
