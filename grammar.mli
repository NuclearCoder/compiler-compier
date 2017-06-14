type word = Epsilon | String of string | Terminal of string | End_marker
type grammar

val mem_first : grammar -> string -> word -> bool
val mem_follow : grammar -> string -> word -> bool


val branch : grammar -> word -> string -> string