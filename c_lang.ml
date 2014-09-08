

let make_keyword key =
	Lexer.({identifier = key;
	regex = Ndfa.String key |> Ndfa.compile })

let keywords = In_channel.read_lines "c_keywords" |> List.map ~f:make_keyword

let whitespace =
	Lexer.({identifier = "white";
	regex = Ndfa.Several (Ndfa.Class Char.is_whitespace) |> Ndfa.compile })

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

let c_tokens = List.fold ~init:[whitespace;identifiers;strings;integers;cpp_comment;c_comment]
		(* The ordering is important here. Since keywords have the form
		of identifiers, it is important they come first in the token
		list, such that the lexer will prefer calling something a
		keyword *)
		~f:(fun acc l -> List.append l acc)
		[keywords;symbols]

let c_string = "while\t{return;} goto lbl; \"A string with a quote\\\" in it\\n\";2+2;//This line is 4\nlbl:100uLL+0x23f8*0345<<=4  \t\ndo->static;int x = 4;/*A C comment\nspans several lines (** / *) */int y = 3;"
