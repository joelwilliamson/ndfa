type regular_language =
        | String of string
	| Union of regular_language list
	| Concat of regular_language list
	| Star of regular_language
	| Wildcard
	| Class of (char -> bool)

type compiled

val check : regular_language -> string -> bool

val compile : regular_language -> compiled

val check_c : compiled -> string -> bool
