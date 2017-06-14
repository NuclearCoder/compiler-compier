type rule = Empty | Sequence of rule list | Repeat of rule | First of rule list | Terminal of string | Rule of string

let with_first c str = String.make 1 c ^ str
let with_last str c = str ^ String.make 1 c

let is_alnum c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
let is_space c = (c == ' ' || c == '\t' || c == '\n' || c == '\r')

let take_while predicate =
	let rec tailrec predicate acc = parser
		| [< 'c when predicate c; tail >] -> tailrec predicate (with_last acc c) tail
		| [< >] -> acc
	in
	tailrec predicate ""

let take_until_char x = take_while (fun c -> c <> x)
let take_while_char x = take_while (fun c -> c = x)
let take_alnum = take_while is_alnum

let flatseq = function
	| r, Empty -> r
	| Sequence a, Sequence b -> Sequence (a @ b)
	| Sequence a, b -> Sequence (List.rev (b :: List.rev a))
	| a, Sequence b -> Sequence (a :: b)
	| a, b -> Sequence [ a; b ]

let flatfst = function
	| First a, First b -> First (a @ b)
	| First a, b -> First (List.rev (b :: List.rev a))
	| a, First b -> First (a :: b)
	| a, b -> First [ a; b ]

let rec rule_tail start = parser
	| [< ''|'; tail >] -> flatfst (start, rule_tail (rule tail) tail)
	| [< >] -> start
and inner_rule = parser [< start = rule; inner = rule_tail start >] -> inner
and rule = parser
	| [< 'c when is_space c; tail >] -> rule tail
	| [< ''"'; str = take_until_char '"'; ''"' ?? "Unclosed terminal token"; tail >] -> flatseq (Terminal str, rule tail)
	| [< 'c when is_alnum c; ref = take_alnum; tail >] -> flatseq (Rule (with_first c ref), rule tail)
	| [< ''('; inner = inner_rule; '')' ?? "Unclosed nested rule"; tail >] -> flatseq (inner, rule tail)
	| [< ''{'; inner = inner_rule; ''}' ?? "Unclosed repeat rule"; tail >] -> flatseq (Repeat inner, rule tail)
	| [< ''['; inner = inner_rule; '']' ?? "Unclosed option rule"; tail >] -> flatseq (flatfst (inner, Empty), rule tail)
	| [< >] -> Empty

let make rule_string = rule (Stream.of_string (with_first '(' (with_last rule_string ')')))

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type ruleset = rule StringMap.t

let empty = StringMap.empty
let singleton name rule = StringMap.singleton name (make rule)
let add name rule set = StringMap.add name (make rule) set
let remove name set = StringMap.remove name set
let contains name set = StringMap.mem name set
let get name set = StringMap.find name set

let ( := ) name rule = name, rule
let ( += ) set (name, rule) = add name rule set

let of_list rules =
	let rec tailrec acc = function
		| [] -> acc
		| (name, rule) :: tail -> tailrec (add name rule acc) tail
	in
	tailrec empty rules

let to_list set = StringMap.bindings set

let rec check_rule set = function
	| Sequence list -> check_rules set list
	| First list -> check_rules set list
	| Repeat rule -> check_rule set rule
	| Rule name when not (contains name set) -> StringSet.singleton name
	| _ -> StringSet.empty
and check_rules set = function
	| [] -> StringSet.empty
	| rule :: tail -> StringSet.union (check_rule set rule) (check_rules set tail)

let list_undefined set =
	let rec iterate acc = function
		| [] -> acc
		| (_ , rule) :: tail -> iterate (StringSet.union (check_rule set rule) acc) tail
	in
	StringSet.elements (iterate (StringSet.empty) (to_list set))

let calculator = of_list [
	"Expr"				:= "Term { MulOp Term }";
	"Term" 			:= "Factor { AddOp Factor }";
	"Factor" 			:= "Integer | Variable | AddOp Factor | \"(\" Expr \")\"";
	
	"Integer" 		:= "Digit { Digit }";
	"Variable" 		:= "Letter { LetterOrDigit }";
	
	"LetterOrDigit"	:= "Letter | Digit";
	"Letter"			:= "\"a\" | \"b\" | \"c\" | \"d\" | \"e\" | \"f\" | \"g\" | \"h\" | \"i\" | \"j\" | \"k\" | \"l\" | \"m\" | \"n\" | \"o\" | \"p\" | \"q\" | \"r\" | \"s\" | \"t\" | \"u\" | \"v\" | \"w\" | \"x\" | \"y\" | \"z\"";
	"Digit"			:= "\"0\" | \"1\" | \"2\" | \"3\" | \"4\" | \"5\" | \"6\" | \"7\" | \"8\" | \"9\"";
	
	"MulOp"			:= "\"*\" | \"/\"";
	"AddOp"			:= "\"+\" | \"-\""
];;

list_undefined calculator;;

to_list calculator;;