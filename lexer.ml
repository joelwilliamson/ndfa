open Core.Std

type token = 
	{
	identifier : string ;
	raw : string
	}

type token_class =
	{
	identifier : string ;
	regex : Ndfa.compiled ;
	}

let get_first_token tcl input : (token option * string) =
	List.map tcl ~f:(fun tc ->
		(tc.identifier,Ndfa.longest_matching_prefix' tc.regex input))
	|> List.filter_map ~f:(fun (i,o) -> match o with
			| Some o -> Some (i,o)
			| None -> None )
	(* This is nlgn in the number of token classes. I expect this to be fine,
	 * but a linear scan could be implemented easily *)
	|> List.sort ~cmp:(fun (_,x) (_,y) -> Int.compare (String.length y) (String.length x))
	|> List.hd
	|> function
		| None -> (None,input)
		| Some (ident,prefix) ->
			let input' = String.drop_prefix input (String.length prefix) in
			(Some {identifier=ident; raw = prefix},input')
			
let rec tokenize tcl input : token list =
	match input with
	| "" -> []
	| input -> let (tok,input') = get_first_token tcl input
		in match tok with
			| None -> failwith ("Couldn't parse line ending with " ^ input')
			| Some tok -> tok :: (tokenize tcl input')

(*let integer = Ndfa.Several (Ndfa.Class Char.is_digit) |> Ndfa.compile ;;
let hex_int = Ndfa.Several (Ndfa.Class (fun c -> (Char.is_digit c) || (Char.lowercase c |> List.mem ['a';'b';'c';'d';'e';'f']))) |> Ndfa.compile ;;
let str = Ndfa.Several (Ndfa.Class Char.is_alpha) |> Ndfa.compile ;;
let lang = [{identifier="int"; regex=integer};{identifier="hex"; regex=hex_int};{identifier="string";regex=str}]*)
