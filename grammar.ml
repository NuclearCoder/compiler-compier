Unix.chdir "/home/nuclearcoder/Documents/CAML/language/cc/";;
#load "rules.cmo";;
open Rules

type word = Epsilon | String of string | Terminal of string | End_marker
module Word =
	struct
		type t = word
		let compare u v = match u, v with (* end marker won't be in the set *)
			| End_marker, End_marker -> 0
			| Epsilon, Epsilon -> 0
			| Epsilon, v -> -1 (* epsilon is the lowest *)
			| u, Epsilon -> 1
			| End_marker, v -> 1 (* $ is the highest *)
			| u, End_marker -> -1
			| Terminal x, Terminal y -> String.compare x y
			| Terminal x, String y -> String.compare x y
			| String x, Terminal y -> String.compare x y
			| String x, String y -> String.compare x y
	end
module WordMap = Map.Make(Word)
module StringMap = Map.Make(String)
module WordSet = Set.Make(Word)

type grammar = {
	start: string;
	rules: ruleset;
	first: WordSet.t StringMap.t; (* rule -> first symbols *)
	follow: WordSet.t StringMap.t; (* rule -> follow symbols *)
	table: string StringMap.t WordMap.t; (* term -> rule -> rule *)
}

let empty_map set =
	let rec create acc = function
		| [] -> acc
		| (name, _) :: tail -> create (StringMap.add name WordSet.empty acc) tail
	in
	create StringMap.empty (Rules.to_list set)

let drop_last_until p = (* take one and while p is true*)
	let rec drop acc = function
		| [] -> acc
		| x :: tail when p x -> drop (x :: acc) tail
		| x :: _ -> acc
	in function
		| [] -> []
		| x :: tail -> x :: drop [] tail

let update_first grammar name set = { grammar with first = StringMap.add name (WordSet.union (StringMap.find name grammar.first) set) grammar.first }

let rec starts_epsilon grammar = function
	| Empty -> true
	| Terminal _ -> false
	| Rule name -> starts_epsilon grammar (Rules.get name grammar.rules)
	| Sequence [] -> true
	| Sequence (first :: _) -> starts_epsilon grammar first
	| First list -> List.exists (starts_epsilon grammar) list
	| Repeat _ -> true

let rec first_list grammar = function
	| [] -> WordSet.empty
	| rule :: tail -> WordSet.union (first_rule grammar rule) (first_list grammar tail)
and first_rule grammar = function
	| Empty -> WordSet.singleton Epsilon
	| Terminal x -> WordSet.singleton (Terminal x)
	| Rule name -> StringMap.find name grammar.first
	| Sequence list -> first_list grammar (drop_last_until (starts_epsilon grammar) list)
	| First list -> first_list grammar list
	| Repeat rule -> WordSet.add Epsilon (first_rule grammar rule)

let get_follow g r = StringMap.find r g.follow

let updatef g r s = { g with follow = StringMap.add r (WordSet.union s (get_follow g r)) g.follow }

let rec follow a r g = match r with
  | Rule b -> updatef g b (get_follow g a)
  | Sequence [Rule b] -> updatef g b (get_follow g a)
  | Sequence (Rule b :: tail) ->
    let first_tail = first_list g tail in
    let s = (match WordSet.mem Epsilon first_tail with
      | true -> WordSet.union (WordSet.remove Epsilon first_tail) (get_follow g a)
      | _ -> first_tail) in
    follow a (Sequence tail) (updatef g b s)
  | Sequence (_ :: tail) -> follow a (Sequence tail) g
  | First (r :: tail) -> follow a (First tail) (follow a r g)
  | Repeat r -> follow a r g
  | _ -> g

let rec build_first grammar =
	let rec increment acc = function
		| [] -> acc
		| (name, rule) :: tail -> increment (update_first acc name (first_rule grammar rule)) tail
	in
	let grammar2 = increment grammar (Rules.to_list grammar.rules) in
	match StringMap.equal (WordSet.equal) grammar.first grammar2.first with
		| true -> grammar
		| _ -> build_first grammar2
		
let build_follow g =
  let rules = Rules.to_list g.rules in
  let equ u v = StringMap.equal WordSet.equal u.follow v.follow in
  let rec iter g = function
    | [] -> g
    | (a, r) :: tail -> iter (follow a r g) tail
  in
  let rec fixed_itr g = match iter g rules with
    | u when equ g u -> g
    | u -> fixed_itr u
  in fixed_itr g

let rec is_epsilon g = function
	| Empty -> true
	| Terminal _ -> false
	| Rule name -> is_epsilon g (Rules.get name g.rules)
	| Sequence [] -> true
	| Sequence list -> List.for_all (is_epsilon g) list
	| First list -> List.exists (is_epsilon g) list
	| Repeat _ -> true

(*let rec simplify a g = function
	| Empty -> [[Epsilon]]
	| Terminal x -> [[String x]]
	| Rule name -> simplify g (Rules.get name g.rules)
	| Sequence [] -> [[Epsilon]]
	| Sequence list -> [List.flatten (List.flatten (List.map (simplify a g) list))]
	| First list -> List.flatten (List.map (simplify g) list)
	| Repeat r -> [Epsilon] :: List.map (fun l -> List.rev (Rule a :: List.rev l)) (simplify g r)*)



(*let build_table_column a r g =
	let rec fill t x = function
		| [] -> t
		| r :: tail -> fill (WordMap.add x (StringMap.add a r (WordMap.find x t)) t) x tail
	in
	let rec iter t r = function
		| [] -> t
		| x :: tail -> iter (fill t x r) r tail
	in
	let list = simplify g r in
	let with_first = iter g.table list (WordSet.elements (StringMap.find a g.first)) in
	{ g with table = match is_epsilon g r with
		| false -> with_first
		| _ -> iter with_first list (WordSet.elements (StringMap.find a g.follow)) }

let build_table g =
	let rec iter g = function
		| [] -> g
		| (a, r) :: tail -> iter (build_table_column a r g) tail
	in
	iter g (Rules.to_list g.rules);;*)

let init_grammar rules start =
	let empty = empty_map rules in
	let g = {
		start = start;
		rules = rules;
		first = empty;
		follow = StringMap.add start (WordSet.singleton End_marker) empty;
		table = WordMap.empty
	} in
	build_follow (build_first g)
	
let grammar = init_grammar Rules.calculator "Expr"

let first_start = StringMap.find grammar.start grammar.first
let follow_start = StringMap.find grammar.start grammar.follow
let first_start_list = WordSet.elements first_start
let follow_start_list = WordSet.elements follow_start

let f = List.map (fun (name, set) -> (name, WordSet.elements set)) (StringMap.bindings grammar.follow)
;;

let mem_first g r w = WordSet.mem w (StringMap.find r g.first)
let mem_follow g r w = WordSet.mem w (StringMap.find r g.follow)

let branch g w r = StringMap.find r (WordMap.find w g.table)
;;