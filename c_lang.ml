

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
	and string_regex = Ndfa.Concat [
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

let strip_whitespace : (Lexer.token list -> Lexer.token list) =
	List.filter ~f:(fun (t:Lexer.token) -> Lexer.(t.identifier) <> "white")

let c_tokens = List.fold ~init:[whitespace;identifiers;strings]
		(* The ordering is important here. Since keywords have the form
		of identifiers, it is important they come first in the token
		list, such that the lexer will prefer calling something a
		keyword *)
		~f:(fun acc l -> List.append l acc)
		[keywords;symbols]



let c_string = "while\t{return;} goto lbl; \"A string with a quote\\\" in it\\n\";lbl:  \t\ndo static"
