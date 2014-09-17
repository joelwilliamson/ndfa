open Core.Std

let make_keyword key =
	Lexer.({identifier = key;
	regex = Ndfa.String key |> Ndfa.compile })

let keywords = In_channel.read_lines "c_keywords" |> List.map ~f:make_keyword

let whitespace =
	Lexer.({identifier = "white";
	regex = Ndfa.Several (Ndfa.Class
		(fun c -> Char.is_whitespace c || c = '\011' || c = '\012'))
		|> Ndfa.compile })

let make_symbol = function
	| identifier::sym::[] ->
		Lexer.({identifier;regex=Ndfa.String sym |> Ndfa.compile})
	| _ -> failwith "Invalid line"

let symbols = In_channel.read_lines "c_punctuation"
		|> List.map ~f:(String.split ~on:' ')
		|> List.map ~f:make_symbol

let identifiers = Lexer.(
	{identifier="IDENT";
	regex=Ndfa.Concat [
		Ndfa.Class (fun c -> Char.is_alpha c || c = '_');
		Ndfa.Star (Ndfa.Class (fun c-> Char.is_alphanum c || c = '_'))]
		|> Ndfa.compile
	})

(* A C string is formatted as follows: The first character is a ".
 * This is followed by any number of regular characters (non- \"), pairs of \\
 * escaped quotes (\"), or other escaped characters \[^\"]. Then is a tailing
 * quote.
 * "([^\"]|\\|\"|\[^\"])*"
 *)
let strings = Lexer.(
	let regular c = c <> '"' && c <> '\\'
	in let string_regex = Ndfa.Concat [
                          Ndfa.String "\"";
                          Ndfa.Star (Ndfa.Union [
                                  Ndfa.Class regular;
                                  Ndfa.String "\\\\";
                                  Ndfa.String "\\\"";
                                  Ndfa.Concat [
                                          Ndfa.String "\\";
                                          Ndfa.Class regular]]);
                          Ndfa.String "\""]|> Ndfa.compile in
	{identifier="string" ;
	regex = string_regex	})

let character = Lexer.(
	let regular c = c <> '\'' && c <> '\\'
	in let string_regex = Ndfa.Concat [
                          Ndfa.String "\'";
                          Ndfa.Star (Ndfa.Union [
                                  Ndfa.Class regular;
                                  Ndfa.String "\\\\";
                                  Ndfa.String "\\\'";
                                  Ndfa.Concat [
                                          Ndfa.String "\\";
                                          Ndfa.Class regular]]);
                          Ndfa.String "\'"]|> Ndfa.compile in
	{identifier="character" ;
	regex = string_regex	})
let integers = Lexer.(
	let ll_suffix  = Ndfa.Union [Ndfa.String "ll"; Ndfa.String "LL"]
	and l_suffix = Ndfa.Union [Ndfa.String "l"; Ndfa.String "L"]
	and unsigned_suffix = Ndfa.Union [Ndfa.String "u" ; Ndfa.String "U"]
	in let integer_suffix = Ndfa.Union [
		Ndfa.Concat [ unsigned_suffix ; Ndfa.Maybe l_suffix] ;
		Ndfa.Concat [ unsigned_suffix ; ll_suffix] ;
		Ndfa.Concat [ l_suffix ; Ndfa.Maybe unsigned_suffix ] ;
		Ndfa.Concat [ ll_suffix ; Ndfa.Maybe unsigned_suffix ]]
	and hex_digit = Ndfa.Class (fun c -> Char.is_digit c ||
			Char.lowercase c |> String.contains "abcdef")
	and octal_digit = Ndfa.Class (String.contains "01234567")
	and nonzero_digit = Ndfa.Class (String.contains "123456789")
	and digit = Ndfa.Class Char.is_digit
	and hex_prefix = Ndfa.Union [ Ndfa.String "0x"; Ndfa.String "0X" ]
	in let hex_constant = Ndfa.Concat [ hex_prefix ; Ndfa.Several hex_digit ]
	and octal_constant = Ndfa.Concat [ Ndfa.String "0" ; Ndfa.Star octal_digit]
	and decimal_constant = Ndfa.Concat [ nonzero_digit ; Ndfa.Star digit]
	in let integer_constant = Ndfa.Concat [
		Ndfa.Union [ decimal_constant; octal_constant; hex_constant] ;
		Ndfa.Maybe integer_suffix ]
	in
	{identifier="integer" ;
	regex=Ndfa.compile integer_constant})

(* This breaks with the assumption that each token is on a single line. It
 * interacts very badly with the line by line approach used to speed up
 * the process. Gwen suggested I look into doing a preprocessing phase to
 * either replace each comment with a space (in compliance with the standard)
 * or to strip newlines from within comments so each one is on a single line.
 *)
let c_comment = Lexer.(
	{identifier = "c_comment" ;
	regex = Ndfa.Concat [ Ndfa.String "/*";
			Ndfa.Star (Ndfa.Union [
				Ndfa.Class ((<>) '/') ;
				Ndfa.Concat [Ndfa.Class ((<>) '*') ;
					Ndfa.String "/"]]) ;
			Ndfa.String "*/" ] |> Ndfa.compile
	})

let cpp_comment = Lexer.(
	{identifier = "cpp_comment" ;
	regex = Ndfa.Concat [ Ndfa.String "//" ;
			Ndfa.Star (Ndfa.Class ((<>) '\n'))]
		|> Ndfa.compile})


let strip_whitespace : (Lexer.token list -> Lexer.token list) =
	List.filter ~f:(fun (t:Lexer.token) -> Lexer.(t.identifier) <> "white")

(* Since all C tokens are expected to be on a single line, this function
 * removes all newlines within a C-style comment
 *)
let compress_comments_mut s : string=
	let rec aux buf n in_comment =
		if Bytes.length buf = n then buf else
		if in_comment then match Bytes.get buf n with
			| '*' -> if Bytes.get buf (n+1) = '/'
				then aux buf (n+2) false
				else aux buf (n+1) true
			| '\n' -> Bytes.set buf n ' ' ;
				aux buf (n+1) true
			| _ -> aux buf (n+1) true
		else if (Bytes.get buf n = '/' && Bytes.get buf (n+1) = '*')
		then aux buf (n+2) true
		else aux buf (n+1) false
	in aux (Bytes.of_string s) 0 false
		

let c_tokens = List.fold ~init:[character;whitespace;identifiers;strings;integers;cpp_comment;c_comment]
		(* The ordering is important here. Since keywords have the form
		of identifiers, it is important they come first in the token
		list, such that the lexer will prefer calling something a
		keyword *)
		~f:(fun acc l -> List.append l acc)
		[keywords;symbols]

let c_string = "while\t{return;} goto lbl; \"A string with a quote\\\" in it\\n\";2+2;//This line is 4\nlbl:100uLL+0x23f8*0345<<=4  \t\ndo->static;int x = 4;/*A C comment\nspans several lines (** / *) */int y = 3;"

let lex_c_string str =
	compress_comments_mut str
	|> String.split_lines
	|> List.map ~f:(Lexer.tokenize c_tokens)
	|> List.join
	|> strip_whitespace

let lex_c_file filename =
	In_channel.read_all filename |> lex_c_string
