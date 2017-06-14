type rule = Empty | Sequence of rule list | Repeat of rule | First of rule list | Terminal of string | Rule of string
type ruleset = rule Map.Make(String).t

val empty : ruleset
val singleton : string -> string -> ruleset
val add : string -> string -> ruleset -> ruleset
val remove : string -> ruleset -> ruleset
val contains : string -> ruleset -> bool
val get : string -> ruleset -> rule

val ( := ) : string -> string -> string * string
val ( += ) : ruleset -> string * string -> ruleset

val of_list : (string * string) list -> ruleset
val to_list : ruleset -> (string * rule) list

val list_undefined : ruleset -> string list

val calculator: ruleset