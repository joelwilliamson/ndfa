open Core_bench.Std

let count_whitespace () = C_lang.lex_c_file "./count_whitespace.c" |> ignore

let dwarf () = C_lang.lex_c_file "./dwarf2out.c" |> ignore

let () =
	[Bench.Test.create ~name:"Count Whitespace" count_whitespace;
		Bench.Test.create ~name:"Dwarf" dwarf]
        |> Bench.make_command
        |> Core.Std.Command.run
