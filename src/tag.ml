module G = Grammar_defs

let join tag1 tag2 = 
  match !G.steminfo with 
  | G.Tag | G.LemmaWithTag | G.StemWithTag 
    -> tag1 ^ !G.tag_delim_string ^ tag2
  | _ -> tag1
	

