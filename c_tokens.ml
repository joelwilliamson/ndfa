type c_token =
        | Tilde | Bang | Hash | Mod | Caret | Ampersand | Star | Lparen | Rparen
        | Minus | Plus | Assign | Lbrace | Rbrace | Lbracket | Rbracket | Colon
        | Semi
        (* There is no quote token. All quotes are part of strings *)
        | Pipe | Backslash | Less | Comma | Greater
        | Period | Question | Slash | Arrow | Inc | Dec | Lshift | Rshift | Lte
        | Gte | Equal | Nequal | And | Or | Ellipse | Star_assign | Slash_assign
        | Mod_assign | Plus_assign | Minus_assign | Lshift_assign | Rshift_assign
        | And_assign | Xor_assign | Or_assign | Double_hash

        | Auto | Enum | Restrict | Unsigned | Break | Extern | Return | Void
        | Case | Float | Short | Volatile | Char | For | Signed | While | Const
        | Goto | Sizeof | Bool | Continue | If | Static | Complex | Default
        | Inline | Struct | Imaginary | Do | Int | Switch | Long | Typedef | Else
        | Register | Union | Double

        | Identifier of string
        | String of string
        | Character of string
        | Number of string
        | Comment of string


let tok_to_fancy (tok:Lexer.token) = let (ident,raw) = (tok.identifier,tok.raw) in
        match ident with
        | "TILDE" -> Tilde | "BANG" -> Bang | "HASH" -> Hash | "MOD" -> Mod
        | "CARET" -> Caret | "AMP" -> Ampersand | "STAR" -> Star
        | "LPAREN" -> Lparen | "RPAREN" -> Rparen | "MINUS" -> Minus | "PLUS" -> Plus
        | "ASSIGN" -> Assign | "LBRACE" -> Lbrace | "RBRACE" -> Rbrace
        | "LBRACKET" -> Lbracket | "RBRACKET" -> Rbracket | "COLON" -> Colon
        | "SEMI" -> Semi
        | "QUOTE" -> failwith "Unmatched quotes forbidden"
        | "APOSTROPHE" -> failwith "Unmatched apostrophes forbidden"
        | "PIPE" -> Pipe | "BACKSLASH" -> Backslash
        | "LT" -> Less | "COMMA" -> Comma | "GT" -> Greater | "PERIOD" -> Period
        | "QUESTION" -> Question | "SLASH" -> Slash | "ARROW" -> Arrow
        | "INC" -> Inc | "DEC" -> Dec | "LSHIFT" -> Lshift | "RSHIFT" -> Rshift
        | "LTE" -> Lte | "GTE" -> Gte | "EQ" -> Equal | "NE" -> Nequal
        | "DAMP" -> And | "DPIPE" -> Or | "ELLIPSE" -> Ellipse
        | "STAR_ASSIGN" -> Star_assign | "SLASH_ASSIGN" -> Slash_assign
        | "MOD_ASSIGN" -> Mod_assign | "PLUS_ASSIGN" -> Plus_assign
        | "MINUS_ASSIGN" -> Minus_assign | "LSHIFT_ASSIGN" -> Lshift_assign
        | "RSHIFT_ASSIGN" -> Rshift_assign | "AMP_ASSIGN" -> And_assign
        | "CARET_ASSIGN" -> Xor_assign | "PIPE_ASSIGN" -> Or_assign
        | "DHASH" -> Double_hash
        | "auto" -> Auto | "enum" -> Enum | "restrict" -> Restrict
        | "unsigned" -> Unsigned | "break" -> Break | "extern" -> Extern
        | "return" -> Return | "void" -> Void | "case" -> Case | "float" -> Float
        | "short" -> Short | "volatile" -> Volatile | "char" -> Char
        | "for" -> For | "signed" -> Signed | "while" -> While | "const" -> Const
        | "goto" -> Goto | "sizeof" -> Sizeof | "_Bool" -> Bool
        | "continue" -> Continue | "if" -> If | "static" -> Static
        | "_Complex" -> Complex | "default" -> Default | "inline" -> Inline
        | "struct" -> Struct | "_Imaginary" -> Imaginary | "do" -> Do
        | "int" -> Int | "switch" -> Switch | "double" -> Double | "long" -> Long
        | "typedef" -> Typedef | "else" -> Else | "register" -> Register
        | "union" -> Union
        | "IDENT" -> Identifier raw
        | "string" -> String raw
        | "character" -> Character raw
        | "integer" -> Number raw
	| "cpp_comment" -> Comment raw
	| "c_comment" -> Comment raw
	| _ -> failwith ("Unknown token type " ^ raw)
