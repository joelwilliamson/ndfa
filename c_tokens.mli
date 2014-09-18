type c_token =
        | Tilde | Bang | Hash | Mod | Caret | Ampersand | Star | Lparen | Rparen
        | Minus | Plus | Assign | Lbrace | Rbrace | Lbracket | Rbracket | Colon
        | Semi
        (* There is no quote token. All quotes are part of strings *)
        | Pipe | Backslash | Less | Comma | Greater        | Period | Question | Slash | Arrow | Inc | Dec | Lshift | Rshift | Lte        | Gte | Equal | Nequal | And | Or | Ellipse | Star_assign | Slash_assign        | Mod_assign | Plus_assign | Minus_assign | Lshift_assign | Rshift_assign        | And_assign | Xor_assign | Or_assign | Double_hash

        | Auto | Enum | Restrict | Unsigned | Break | Extern | Return | Void        | Case | Float | Short | Volatile | Char | For | Signed | While | Const
        | Goto | Sizeof | Bool | Continue | If | Static | Complex | Default
        | Inline | Struct | Imaginary | Do | Int | Switch | Long | Typedef | Else
        | Register | Union | Double
        | Identifier of string
	| String of string
	| Character of string
	| Number of string
	| Comment of string
 
val tok_to_fancy: Lexer.token -> c_token
