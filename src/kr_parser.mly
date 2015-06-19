/* File kr_parser.mly uses kr.ml */
%token <int> INT
%token OPEN CLOSE /* for bracketing */

%token AND         /* path delimiter */
%token SUB         /* embedding */
%token DEF         /* signature definition */ 
%token EOP         /* end of pattern */ 
%token MAY         /* underspecification prefix */
%token <string> EDGE    /* name of the edge */
%start main             /* the entry point */
%type <Kr.signature -> Kr.t> main
%%
main: 
| EOP              { raise End_of_file }
| EDGE matrix EOP  { Utils.carp 3 "lemma\n"; function s -> 
    let fs = $2 ( Kr.sig_get s "", Kr.snode_get ) in
    Kr.Lemma($1,fs) }
| matrix EOP       { Utils.carp 3 "just\n"; function s -> $1  ( Kr.sig_get s "", Kr.snode_get ) }
| DEF EDGE matrix EOP { Utils.carp 3 "sign\n"; function s -> $3  ( Kr.sig_get s $2, Kr.snode_get ) }
| EDGE DEF matrix EOP { Utils.carp 3 "%sdefined\n" $1; function s -> $3  ( Kr.sig_add s $1, Kr.snode_add ) }
    ;
matrix:
| OPEN subtree CLOSE   { $2 }
| matrix matrix        { function x -> Kr.join ($1 x) ($2 x) }
    ;
subtree:
| EDGE  {  
  function snode, snodef ->
    let ind, node = snodef snode $1 in
    Kr.Node( Kr.bv_set 0 ind, snode, [ind, Kr.Node( Kr.bv_all_zero, node, [] )] )
}
| MAY  {  
  function snode, _ ->
    Kr.Node( Kr.bv_all_one, snode, [] )
}
| MAY EDGE { 
  function snode, snodef ->
    let ind, _ = snodef snode $2  in
    Kr.Node( Kr.bv_set 0 ind, snode, [])
}
| EDGE matrix { 
  function snode, snodef -> 
    let ind, node = snodef snode $1 in 
    Kr.Node( Kr.bv_set 0 ind, snode, [ind, ($2 (node, snodef))])
}
    ;
edge: 
| EDGE { $1 }  
