(* uses Printf Output Grammar_defs *)

module G = Grammar_defs 

exception Oops

exception Empty

let comp_cnt = ref 0
let covered = ref false 
let coverage = ref G.IndexSet.empty
  
(* minunits is the clever algorithm that folds sets into the minimal number of maximal 
   sets unions of which can give the original sets
   and marks each original 
 *)
let minunits parbv flag_str lst = 
  if G.IndexSet.is_empty parbv then lst else begin
    (* Utils.carp 5 "Starting minunits on new paradigm bitvector\n"; *)
    let minunits_2  comptail ( flag, comp, flag_str_list )  = 
      try 
	let intersection = G.IndexSet.inter comp parbv in
	if G.IndexSet.is_empty intersection then raise Empty;
	flag_str := !flag_str ^ flag;
	(* Utils.carp "Non-empty intersection\n";*)
	try 
	  let xorsection = G.IndexSet.diff comp intersection in
	  if G.IndexSet.is_empty xorsection then raise Empty;
	  (* Utils.carp 1 "Non-empty xorsection\n"; *)
	  let newflag = !Output.get_flag () in 
	  incr comp_cnt;
	  (* Utils.carp 1 "component %d - %s\n" !comp_cnt newflag; *)
	  let add_newflag x = x := !x ^ newflag in
	  List.iter (add_newflag) flag_str_list;
	  ( flag, intersection, ( flag_str :: flag_str_list  ) )
	  :: ( newflag, xorsection, flag_str_list )
	  :: comptail
	with Empty ->
	  ( flag, comp, ( flag_str :: flag_str_list  ) ) :: comptail
      with Empty ->
	( flag, comp, flag_str_list ) :: comptail
    in
    match lst with 
    | [] -> 
	(* Utils.carp 5 "start\n"; *)
	let firstflag = !Output.get_flag () in
	incr comp_cnt;
	(* Utils.carp 1 "component %d - %s\n" !comp_cnt firstflag; *)
	flag_str := firstflag;
	covered := false;
	coverage := parbv; 
	[ firstflag, parbv, [ flag_str ] ]
    | _ -> 
	if !covered 
	then List.fold_left (minunits_2) [] lst 
	else begin
	  try 
	    let newcomp = G.IndexSet.diff parbv !coverage in
	    (* Utils.carp 1 "this one is *not* covered\n";*)
	    if G.IndexSet.is_empty newcomp then raise Empty;
	    coverage := G.IndexSet.union !coverage parbv ;
	    if G.IndexSet.cardinal !coverage = !G.aff_output_count
	    then covered := true;
	    let newflag =  !Output.get_flag () in 
	    incr comp_cnt;
	    (* Utils.carp 1 "component %d - %s\n" !comp_cnt newflag; *)
	    flag_str := !flag_str ^ newflag;
	    let newtail = List.fold_left (minunits_2) [] lst in
	    (* Utils.carp "minunits_2 done\n";*)
	    ( newflag, newcomp, flag_str :: [] ) ::  newtail
	  with Empty ->  
	    (* Utils.carp 1 "this one *is* covered\n"; *)
	    let newtail = List.fold_left (minunits_2) [] lst in
	    (* Utils.carp "minunits_2 done\n";*)
	    newtail
	end
  end



let create affix_channel = 
  
  let rule_cnt = !G.aff_output_count in
  let list = ref !G.aff_output_list in
  
  let aff_outputs = Array.of_list !G.aff_output_list in 
  G.aff_output_list := [];

  comp_cnt := 0;
  covered := false ;
  coverage := G.IndexSet.empty ;
  
  let apply _ (_, index_set, freq_ref, flag_str) (i, lst, alias_lst) = 
    if !freq_ref > 0 
    then (
      incr G.index_set_count;
      let lst = minunits index_set flag_str lst in
      let alias_lst = flag_str :: alias_lst in
      (i+1, lst, alias_lst)
     )
    else (i, lst, alias_lst)
  in

  Utils.carp 2 "Calculating components\n";
  (* folding over flag hash indexes *)
  let n, components, alias_lst = Hashtbl.fold (apply) G.cont_tbl (0,[],[]) in
  
  Hashtbl.clear G.cont_tbl;
  (*
  let dump_aliases i flag_str  = 
    let is = string_of_int i in
    Printf.fprintf affix_channel "AF %s\n" !flag_str;
    flag_str := is;
    i+1
  in
  let aliases = true in
  if aliases then (
    Printf.fprintf affix_channel "AF %d\n" n;
    ignore(List.fold_left (dump_aliases) 1 alias_lst);
   );*)

  Utils.carp 2 "Dumping flags with associated rule sets\n";

  let dump_component2 (flag, parbv, parlist) = 
    
    let process i (p, s) = 
      let i' = rule_cnt - i in
      let a = aff_outputs.(i') in 
      (* collect identical suffix and prefix parts of circumfixes *)
      match a 
      with 
      | G.Aff_output(G.Suffix(_,_,_), _, _, _, _,_) -> 
	  (p, (a :: s) )
      | G.Aff_output(G.Prefix(_,_,_), _, _, _, _, _) -> 
	  ( (a :: p), s)
      | G.Aff_output(G.Dummy(G.Lexis(w)), _, _, _, _, _) -> 
	  let flag_ref = try
	    Utils.lazy_carp 2 (lazy (Printf.eprintf "retrieve dummy object flag\n"));
	    Hashtbl.find G.compound_flag_tbl w
	  with Not_found ->
	    Utils.carp 0 "impossible: morph? '%s' not entered in table\n" w;
	    raise (Failure "oops")
	  in
	  flag_ref := flag;
	  (p, s)
      | _ -> 
	  Utils.carp 0 "rule impossible to output\n";
	  raise (Failure "oops")
    in
    let prefixes, suffixes = G.IndexSet.fold (process) parbv ([],[]) in
    (* create index of circumfix-bound  prefixes *)
    let dump_free_final (i, out) f =
      let f_out = Output.pretty_print_final flag f in
      if f_out = "" 
      then (i, out) 
      else (i + 1, f_out :: out)
    in
    let dump_rule r = 
      Printf.fprintf affix_channel "%s\n" r;
      (* Printf.eprintf "%s\n" r *)
    in
    let n, out = List.fold_left (dump_free_final) (0, []) prefixes in
    if n <> 0 then (
      Utils.carp 4 "%d prefixes for this flag\n" n;
      Printf.fprintf affix_channel "\nPFX %s Y %d\n" 
	flag n;
      List.iter (dump_rule) out;
     );
    let n, out = List.fold_left (dump_free_final) (0, []) suffixes in
    if n <> 0 then (
      Utils.carp 5 "%d suffixes for this flag\n" n;
      Printf.fprintf affix_channel "\nSFX %s Y %d\n" 
	flag n;
      List.iter (dump_rule) out;
     )
  in 
  List.iter (dump_component2) components;
  Utils.carp 5 "done\n";
  


