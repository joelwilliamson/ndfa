type token =
	{
	identifier : string ;
	raw : string ;
	}

type token_class  =
	{
	identifier : string ;
	regex : Ndfa.compiled ;
	}

val tokenize : token_class list -> string -> token list 
