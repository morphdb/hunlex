 (* uses Grammar_defs Grammar_parser Printf Lexing *)
{
module P = Grammar_parser
module G = Grammar_defs

type inifin = INI | FIN | SUB

let inifin = ref INI

exception Skip

let escaped_quote_regexp = 
  let str = String.create 2 in
  str.[0] <- '\\';
  str.[1] <- '"';
  Str.regexp (String.escaped str)
let quote_string = String.make 1 '"'

let process_escaped_quote x = 
  Str.global_replace escaped_quote_regexp quote_string x


let regexpify x = 
  let rec substitute orig result = 
    try 
      let from = String.index orig '<' in
      let till = try String.index_from orig from '>'
      with Not_found -> failwith "invalid pattern x"
      in
      let len = till - from in 
      let found = String.sub orig (from+1) (len-1) in
      let replace = try 
	Hashtbl.find G.pat_index found 
      with Not_found -> 
	failwith 
	  (Printf.sprintf "regex pattern '%s' is undefined in %s" found x)
      in
      let neworig = String.sub orig (till+1) ((String.length orig) - till - 1) in 
      let newresult = result ^ (String.sub orig 0 from) ^ replace in 
      substitute neworig newresult
    with Not_found ->  result ^ orig
  in
  let x_s = substitute x "" in
  Utils.carp 3 "regexp pattern '%s' resolved as '%s'\n" x x_s;
  x_s
    
type state = START | DEFINE | REGEXP | REGEXP_DEFINITION | CLIP | FS | USAGE | ID | IF | MATCH | FREE | KEEP | SKIP | FILTER | RULES | PASS | WITH | RECURSION | COMPOUND_CAT | COMPOUND_MOD | COMPOUND_MOD_DEFINE | COMPOUND_CAT_DEFINE | TAG of int | GUESS | FORBIDDEN

let ( keywords : (string , state) Hashtbl.t ) =  (Hashtbl.create 53)

let kw_map = [
  ("RECURSION", RECURSION);
  ("DEFINE", DEFINE);
  ("REGEXP", REGEXP);
  ("CLIP", CLIP);
  ("IF", IF);
  ("MATCH", MATCH);
  ("REPLACE", MATCH);
  ("OUT", ID);
  ("FREE", FREE);
  ("PASS", PASS);
  ("KEEP", KEEP);
  ("FILTER", FILTER);
  ("DEFAULT", FILTER);
  ("JOIN", FS);
  ("FS",   FS);
  ("USAGE", USAGE);
  ("MORPH", START);
  ("VARIANT", RULES);
  ("WITH", WITH);
  ("COMPOUND_CAT", COMPOUND_CAT);
  ("COMPOUND_MOD", COMPOUND_MOD);
  ("COMPOUND", COMPOUND_MOD_DEFINE);
  ("GUESS", GUESS);
  ("FORBIDDEN", FORBIDDEN);
]

let _ = List.iter (fun (keyword, state) -> Hashtbl.add keywords keyword state) kw_map

let state = ref START

let map_block_item x token morphskip ruleskip lexbuf = 
  ( match !state with 
     | SKIP ->
	 token lexbuf
     | REGEXP -> 
	   if Hashtbl.mem G.pat_index x 
	   then ( 
	     Utils.lazy_carp 1 
	       (lazy (Printf.eprintf "Warning: multiple definition of pattern '%s': skip this one\n" x)); 
	     state := START;
	     morphskip lexbuf
	    ) else ( 
	     state := REGEXP_DEFINITION; 
	     Utils.lazy_carp 2 (lazy (Printf.eprintf "Pattern '%s' declared\n" x)); 
	     P.REGEXP(x) 
	    )
       | REGEXP_DEFINITION ->
	   P.REGEXP_STRING(regexpify x)
       | DEFINE ->
	   if Hashtbl.mem G.abbr_index x 
	   then ( 
	     Utils.lazy_carp 1 
	       (lazy (Printf.eprintf "WARNING: multiple definition of abbreviation '%s': skip this one\n" x)); 
	     state := START;
	     morphskip lexbuf
	    ) else (
	     state := ID; 
	     Utils.lazy_carp 2 
	       (lazy (Printf.eprintf "Abbreviation '%s' declared\n" x)); 
	     P.DEFINE(x) 
	    )
       | ID -> (
	   Utils.lazy_carp 3 (lazy (Printf.eprintf "'%s' " x));
	   try 
	     let abbrev = Hashtbl.find G.abbr_index x in
	     Utils.carp 3 "(OUTFUNC)\n";
	     P.OUTFUNC(abbrev)
	   with Not_found -> 
	     try 
	       let morpheme = Hashtbl.find G.morph_index x in 
	       Utils.lazy_carp 4  (lazy (Printf.eprintf "(morph %d)\n" morpheme.G.id));
	       (* well, we should not skip 'lexical' affixes as output spec *)
	       (* if !r && !current_level <> 0 && aff.G.level = 0 then raise Skip; *)
	       P.MORPH(morpheme)
	     with Not_found -> 
	       try 
		 let i = Hashtbl.find G.phono_index x in 
		 Utils.lazy_carp 4 (lazy (Printf.eprintf "(feature %d)\n" i));
		 P.PHONO(i)
	       with Not_found -> 
		 Utils.lazy_carp 1 (lazy (Printf.eprintf "Warning: unknown entity '%s' skipped\n" x)); 
		 token lexbuf 
	  )
       | IF -> (
	   Utils.lazy_carp 4 (lazy (Printf.eprintf "%s" x)); 
	   try 
	     let i = Hashtbl.find G.phono_index x in 
	     Utils.lazy_carp 4  (lazy (Printf.eprintf " (feature %d)\n" i));
	     P.PHONO_COND(i)
	   with Not_found -> 
	     Utils.lazy_carp 1 (lazy (Printf.eprintf "Warning: unknown phono feature '%s' skipped\n" x)); 
	     token lexbuf 
	  )
       | MATCH -> ( 
	   if x = "." then (
	     Utils.lazy_carp 3 (lazy (Printf.eprintf "%s" x)); 
	     P.MATCH( None ) 
	    ) else (
	     let re =  (regexpify x) in
	     let r = try 
	       Str.regexp re
	     with _ ->
	       Utils.carp 0 "Ill-formed regular expression '%s' derived from '%s'" re x ; 
	       raise Parsing.Parse_error
	     in 
	     Utils.carp 3 " (pattern)\n"; 
	     P.MATCH( Some( re, r ) )
	    )
	  )
       | USAGE -> (
	   try 
	     let q  = Hashtbl.find G.usage_index x in
	     Utils.lazy_carp 3 (lazy (Printf.eprintf "(%s)\n" x));
	     P.USAGE(q)
	   with Not_found -> 
	     token lexbuf
	  )
       | CLIP -> (
	   try 
	     let clip = int_of_string x in
	     P.CLIP( clip )
	   with _ -> 
	     Utils.carp 0 "Invalid number '%s'" x;
	     raise Parsing.Parse_error
	  )
       | FREE -> ( 
	   match x with 
	   | "true"  -> P.FREE(true)
	   | "false" -> P.FREE(false)
	   | _ -> 
	       Utils.carp 0 "boolean value 'true' or 'false' expected\n";
	       raise Parsing.Parse_error
	  )
       | PASS -> ( 
	   match x with 
	   | "true"  -> P.PASS(true)
	   | "false" -> P.PASS(false)
	   | _ -> 
	       Utils.carp 0 "boolean value 'true' or 'false' expected\n";
	       raise Parsing.Parse_error
	  )
       | START -> (
	   if !G.lexicon then (
	     Utils.lazy_carp 6 (lazy (Printf.eprintf "%s (lemma)\n" x));
	     state := ID;
	     P.MORPH(G.make_lemma x)
	    ) else (
	     try
	       let morph = 
		 try 
		   let a = Hashtbl.find G.morph_index x in
		   if a.G.init 
		   then (Utils.lazy_carp 1 
			   (lazy (Printf.eprintf "Warning: multiple definitions of '%s': redefinition " x));
			 raise Skip)
		   else (
		     a.G.init <- true; 
		     a
		    )
		 with Not_found -> 
		   Utils.lazy_carp 1 
		     (lazy (Printf.eprintf "Warning: undeclared morph '%s' " x));
		   raise Skip
	       in
	       Utils.lazy_carp 3 (lazy (Printf.eprintf "%s (morph)\n" x));
	       state := ID;
	       P.MORPH(morph)
	     with Skip -> 
	       Utils.carp 1 "skipped\n";
	       morphskip lexbuf
	    )
	  )
       | RECURSION -> (
	   P.RECURSION(int_of_string x)
	  )
       | WITH -> (
	   if !G.lexicon then (
	     Utils.carp 0 "there can be no substitutions in the lexicon\n";
	     raise Parsing.Parse_error
	    ) else P.SUBST(x)
	  ) 
       | RULES -> (
	   if !G.lexicon then (
	     Utils.lazy_carp 4 (lazy (Printf.eprintf "%s (variant)\n" x));
	     state := ID;
	     P.LEXIS(x)
	    ) else (
	     Utils.lazy_carp 4 (lazy (Printf.eprintf "%s (suppletion) ... consider putting it in the lexicon\n" x));
	     state := ID;
	     P.SUPPL(x)
	    )
	  )
       | TAG tag_index -> (
	   (* default index for tags *)
	   P.TAG_STRING(tag_index,x) 
	  )
       | FS -> (
	   try
	     let fs =  Fs.read x in
	     P.FS(Some(fs))
	   with _ ->
	     Utils.carp 0 "Invalid feature structure '%s'\n" x;
	     raise Parsing.Parse_error
	  )
       | KEEP -> (
	   try 
	     let i = Hashtbl.find G.phono_index x in 
	     Utils.lazy_carp 4 (lazy (Printf.eprintf "(KEEP PHONO %d)\n" i));
	     P.KEEP(i)
	   with Not_found -> 
	     Utils.lazy_carp 1 (lazy (Printf.eprintf "Warning: unknown phono feature '%s' in KEEP block skipped\n" x)); 
	     token lexbuf 
	  )
       | FILTER -> (
	   try 
	     let i = Hashtbl.find G.phono_index x in 
	     Utils.lazy_carp 4 (lazy (Printf.eprintf  "(FILTER PHONO %d)\n" i));
	     P.FILTER(i)
	   with Not_found -> 
	     Utils.lazy_carp 1 (lazy (Printf.eprintf "Warning: unknown phono feature '%s' in FILTER block skipped\n" x)); 
	     token lexbuf 
	  )
       | COMPOUND_CAT -> (
	   let x = "COMPOUND_CAT: " ^ x in
	   try 
	     let morpheme = Hashtbl.find G.morph_index x in 
	     Utils.lazy_carp 4  (lazy (Printf.eprintf "(compound category %d)\n" morpheme.G.id));
	     P.MORPH(morpheme)
	   with Not_found -> 
	     Utils.lazy_carp 1 (lazy (Printf.eprintf "Warning: unknown compound category '%s' skipped\n" x)); 
	     token lexbuf 
	  )
       | COMPOUND_MOD -> (
	   let x = "COMPOUND_MOD: " ^ x in
	   try 
	     let morpheme = Hashtbl.find G.morph_index x in 
	     Utils.lazy_carp 4  (lazy (Printf.eprintf "(compound category %d)\n" morpheme.G.id));
	     P.MORPH(morpheme)
	   with Not_found -> 
	     Utils.lazy_carp 1 (lazy (Printf.eprintf "Warning: unknown compound category '%s' skipped\n" x)); 
	     token lexbuf 
	  )
       | GUESS -> (
	   if (
	     match x with 
	     | "true"  -> true
	     | "false" -> false
	     | _ -> 
		 Utils.carp 0 "boolean value 'true' or 'false' expected\n";
		 raise Parsing.Parse_error
	    )
	   then	
	     let x = "__GUESS@DUMMY__" in
	     let guess = 
	       try 
		 let guess = Hashtbl.find G.morph_index x in
		 Utils.lazy_carp 2  (lazy (Printf.eprintf "(guess)\n"));
		 guess
	       with Not_found ->
		 let guess = G.make_compound_morph x in
		 Hashtbl.add G.morph_index x guess;
		 Hashtbl.add G.compound_flag_tbl x G.guess_flag;
		 Utils.lazy_carp 2 (lazy (Printf.eprintf "created guesser dummy morph\n"));
		 guess
	     in
	     P.MORPH(guess)
	   else
	     token lexbuf
	  )
       | COMPOUND_MOD_DEFINE -> (
	   let x = "COMPOUND_MOD: " ^ x in
	   state:= COMPOUND_CAT_DEFINE;
	   let _ = 
	     try 
	       ignore (Hashtbl.find G.morph_index x)
	     with Not_found ->
	       let morpheme = G.make_compound_morph x in
	       Hashtbl.add G.morph_index x morpheme;
	   in
	   Utils.lazy_carp 4  (lazy (Printf.eprintf "(compound modifier category %s)\n" x));
	   
	   P.COMPOUND_MOD(x)
	  )
       | COMPOUND_CAT_DEFINE -> (
	   let x = "COMPOUND_CAT: " ^ x in
	   let _ = 
	     try 
	       ignore (Hashtbl.find G.morph_index x)
	     with Not_found ->
	       let morpheme = G.make_compound_morph x in
	       Hashtbl.add G.morph_index x morpheme
	   in
	   Utils.lazy_carp 4  (lazy (Printf.eprintf "(compound head category %s)\n" x));
	   P.COMPOUND_CAT(x)
	  )
   )
	     


}
let identchar = [^'#' ' ' '\t' '\n' ';' ',' '\r' '!' '+']
let word = identchar+
let no_quote_string = [^'"']+
let potentially_escaped_string = ([^'"']*('\\''"')?)*[^'"']*
let ws = [' ' '\t' '\n' '\r']
let input = ['+']
let zero = input
rule token = parse 
  ws+     { token lexbuf }     (* skip all blanks *)
| '#'[^'\n']*'\n'      { token lexbuf }     (* skip comments till end of line *)
| ','                  { Utils.carp 3  "DELIM\n"; state := RULES; P.DELIM }
| ';'                  { Utils.carp 3  "END\n\n"; state := START; P.END }
| eof | "ENDINPUT:"    { Utils.carp 3  "END OF INPUT\n";  if !G.lexicon then raise End_of_file else P.EOF }
| '"' (no_quote_string as x) '"' 
    { 
      Utils.lazy_carp 3 (lazy (Printf.eprintf "QUOTED %s\n" x)); 
      map_block_item x (token) (morphskip) (ruleskip) lexbuf;
    }
| '"' (potentially_escaped_string as x) '"' 
    { 
      let x = process_escaped_quote x in
      Utils.lazy_carp 3 (lazy (Printf.eprintf "QUOTED %s\n" x)); 
      map_block_item x (token) (morphskip) (ruleskip) lexbuf;
    }
| '!'                    { Utils.carp 3 "NOT\n"; P.NOT }
| ((word as p) input (word as s)) as x
    { 
      Utils.carp 3  "%s+%s (CIRCUMFIX)\n" p s; 
      inifin := FIN; (* this can be modified *)
      state := ID;
      P.CIRCUMFIX(p,s);
    }
| zero   
    { 
      Utils.carp 3 "ZERO AFFIX"; 
      state := ID;
      P.ZERO 
    }
| input (word as x)      
    { 
      Utils.carp 3 "%s (SUFFIX)\n" x; 
      state := ID;
      P.SUFFIX(x)
    }
| (word as x) input
    { 
      Utils.carp 3 "%s (PREFIX)\n" x; 
      state := ID;
      P.PREFIX(x)
    }
| word as x ':'
    {
     match x with (* hardwired keywords *)
     | "INI" -> Utils.carp 3 "INI\n"; P.INI
     | "FIN" -> Utils.carp 3 "FIN\n"; P.FIN
     | "FORALL" -> Utils.carp 3 "FORALL\n"; P.FORALL
     | _ ->
	 try 
	   assert ( Utils.lazy_carp 6 (lazy (Printf.eprintf "Trying reserved keyword '%s'\n" x)); true );
	   let s = Hashtbl.find keywords x in 
	   let s = if s <> FS || !G.use_fs then s else SKIP in
	   state := s;
	   token lexbuf
	 with Not_found ->
	   try
	     assert ( Utils.lazy_carp 6 (lazy (Printf.eprintf "Trying tag keyword '%s'\n" x)); true );
	     let tag_index = List.assoc x !G.tag_index in 
	     let s = if tag_index = 0 then SKIP else TAG tag_index in 
	     state := s;
	     token lexbuf
	   with Not_found ->
	     Utils.lazy_carp 1 
	       (lazy (Printf.eprintf "Warning: Unknown keyword '%s': skip this block\n" x));
	     state := SKIP;
	     token lexbuf
   }
| word as x
    {
     map_block_item x (token) (morphskip) (ruleskip) lexbuf
   }

and morphskip = parse
    ([^';' '#']*('#' [^'\n']* '\n')*)*';'  { Utils.carp 5 "Skip rest of entry\n"; token lexbuf } 


and ruleskip = parse
([^',' ';' '#']*(ws* '#' [^'\n']* '\n')*)[',' ';'] ([^',' ';' '#']*('#' [^'\n']* '\n')*)[',' ';']
('#' [^'\n']* '\n')*
  { token lexbuf } (* skip whole rule *)




