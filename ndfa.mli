type regular_language =
        | String of string
	| Union of regular_language list
	| Concat of regular_language list
	| Star of regular_language
	| Wildcard
	| Class of (char -> bool)
	| Maybe of regular_language
	| Several of regular_language

type compiled

(** Does the string match the given regular language? **)
val check : regular_language -> string -> bool

(** Convert the language to an intermediate representation that allows faster
  * calls to check. check makes this call each time it is invoked, so using
  * this function will almost always be faster if you check multiple strings. **)
val compile : regular_language -> compiled

(** Does the string match the compiled regular expression? **)
val check' : compiled -> string -> bool

(** Execution contexts are used when we want to check whether a series of
  * strings, each containing the previous as a prefix, all match the language.
  * This is most notably used in lexing, when a common task is to find the
  * longest string which matches any of a set of regexes **)
type exec_context

(** Get the execution context for a given language or compiled language **)
val get_char_checker : regular_language -> exec_context
val get_char_checker' : compiled -> exec_context

(** Advance the check by one character, returning the new context. The first
  * bool indicates if the context might match, given an appropriate sequence.
  * The second bool indicates if the currently provided sequence of chars is
  * matched. **)
val check_char : exec_context -> char -> (exec_context * bool * bool)

(** Find the longest prefix of the argument string that is matched by the
  * regular expression. **)
val longest_matching_prefix : regular_language -> string -> string option
val longest_matching_prefix' : compiled -> string -> string option
