(*
#require "p0";;
#require "re";;
#require "re.emacs";;
*)

open P0

(* redefine the regular expression parsers to use ocaml-re, which has
   longest match semantics *)


module Re_ = struct

  open Re

  type re = { s_:string; re_:Re.t }

  let literal s = { s_=s; re_=str s }


  let string_match ~re ~off s =
    seq[start;re.re_] |> longest |> group |> compile |> fun re -> 
    Re.exec_opt ~pos:off re s |> function
    | None -> None
    | Some groups ->
      let (i,j) = Group.offset groups 0 in
      assert(i=off);
      Some j


  (* should be same as above; less efficient but semantics clear *)
  let string_match ~re ~off s =
    seq[start;re.re_] |> longest |> group |> compile |> fun re' -> 
    try
      Re.matches ~pos:off re' s |> function
      | [] -> None
      | [s'] -> Some(off + String.length s')
      | s'::_ -> 
        (* multiple longest matches *)
        Some(off + String.length s')
    with Not_found -> None


  let search_forward ~re ~off s =
    re.re_ |> shortest |> group |> compile |> fun re -> 
    Re.exec_opt ~pos:off re s |> function
    | None -> None
    | Some groups ->
      let (i,j) = Group.offset groups 0 in
      Some i

  ;;

  let _ = string_match ~re:({s_="char a"; re_=char 'a'}) ~off:1 "bac"
  let _ = string_match ~re:({s_="char a2"; re_=char 'a'}) ~off:0 "bac"

  let _ = search_forward ~re:({s_="char a3"; re_=char 'a'}) ~off:1 "cccababa"
end

open Re_

module P0_re = P0.Make(Re_)

open P0_re


(* grammar of grammars ---------------------------------------------- *)

let _ = Re_.string_match ~re:{s_=__LOC__; re_=(Re_emacs.re "[ \n]*")} ~off:0 "abc"

(* FIXME may want to rename P0_re.re *)
let re s = {s_=s; re_=Re_emacs.re s} |> P0_re.re


let comm = a "(*" -- upto_a "*)" -- a "*)"  (* FIXME nested comments *)
(* ws: 0 or more; re is hopefully longest match (not true for Str) *)
let rec ws s = (re "[ \n]*") --- (opt (comm --- ws)) @@ s
let nt = re "[A-Z]+" 
let lit = 
  let sq = "'" in
  let dq = "\"" in
  ((a sq -- upto_a sq -- a sq) ||
  (a dq -- upto_a dq -- a dq))
  |>> fun x -> return (_3 x)
let tm = 
  lit |>> (fun x -> return @@ `Lit x) ||
  (* allow a single question mark for terminals *)
  (a"?" -- re"[a-z_][a-zA-Z0-9]*") |>> fun (x,y) -> return @@ `Qu(x,y)
let sym = 
  (nt |>> fun x -> return (`NT x)) || 
  (tm |>> fun x -> return (`TM x))
(*
let var_eq = 
  let v = re "[a-z][a-z0-9]*" in
  let v_eq = v -- a"=" in
  opt v_eq -- sym |>> fun (v,s) -> return (v,s)
*)
let syms = plus ~sep:ws sym
(* let vnames = plus ~sep:(a",") (re"[a-z_][a-z0-9_]*")
//let syms' = syms -- opt (ws -- a"//" -- opt (ws -- vnames)) *)
let bar = ws -- a "|" -- ws 
let rhs = plus ~sep:bar syms  (* plus and star are greedy *)
let rule = 
  sym -- (ws -- a "->" -- ws) -- rhs |>> fun x -> 
  _3 x |> fun (sym,_,rhs) -> return (sym,rhs)
let rules = star ~sep:(ws -- a";" -- ws) rule 
let grammar = ws -- rules -- ws |>> fun x -> _3 x |> fun (_,x2,_) -> return x2


module X_ = struct
  let example = {|

(* the expressions we want to parse at top-level *)
S -> ?w? DEFN ?w? ?eof?
| ?w? TYPEDEFINITIONS ?w? ?eof?
| ?w? TYPEXPR ?w? ?eof?

|}

  let _ = grammar example

end
