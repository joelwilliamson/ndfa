type regular_language =
        | String of string
	| Union of regular_language * regular_language
	| Concat of regular_language * regular_language
	| Star of regular_language
	| Wildcard
	| Class of (char -> bool)

type compiled

val check : regular_language -> string -> bool

val compile : regular_language -> compiled

val check_c : compiled -> string -> bool
