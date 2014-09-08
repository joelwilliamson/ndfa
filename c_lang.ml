

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

let strip_whitespace : (Lexer.token list -> Lexer.token list) =
	List.filter ~f:(fun (t:Lexer.token) -> Lexer.(t.identifier) <> "white")

let c_tokens = List.append keywords (whitespace::symbols)

let c_string = "while\t{return;} goto ; :  \t\ndostatic"
