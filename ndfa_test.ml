open Ndfa
open OUnit2
open Core.Std

let () =
	let please_recognize test str _ = assert_bool ("Failed to recognize " ^ str) (test str)
	and dont_recognize test str _ = assert_bool ("Recgnized " ^ str ^ " incorrectly") (not (test str)) in
	let name first rest = Concat
				[Union
					[String (Char.uppercase first |> Char.to_string);
					String (Char.to_string first)];
				String rest] in
	let hyphen_name (f1,r1) (f2,r2) = Concat [name f1 r1; String "-"; name f2 r2] in
	let j_l = name 'j' "oel"
	and g_l = name 'g' "wen"
	and stewart = name 's' "tewart"
	and will = name 'w' "illiamson"
	and int_l = Concat [Class Char.is_digit; (Star (Class Char.is_digit))]
	in let j_dot_l = Concat [j_l; Star Wildcard]
	in let jg_m = compile (Union [j_l; g_l])
	in let several_jg_m = compile (Star (Union [j_l; g_l]))
	in let uppercase_joel_test = please_recognize (check j_l) "Joel"
	and lowercase_joel_test = please_recognize (check j_l) "joel"
	and invalid_joel_test = dont_recognize (check j_l) "jOel"
	and uppercase_gwen_test = please_recognize (check' jg_m) "Gwen"
	and lowercase_j_2_test = please_recognize (check' jg_m) "joel"
	and misspelled_gwen_test = dont_recognize (check' jg_m) "Gewn"
	and several_jg_test = please_recognize (check' several_jg_m) "JoelGwengwenjoel"
	and several_jg_spaces = dont_recognize (check' several_jg_m) "Joel joel Gwen gwen"
	and spaces_2 = please_recognize (check (Star (Concat [Union [j_l;g_l]; Union [String " ";String ""]]))) "Gwen joelJoel gwen"
	and simple_dot = please_recognize (check Wildcard) "x"
	and joel_trailing = please_recognize (check j_dot_l) "Joel is the programmer of this module"
	and number = please_recognize (check int_l) "123"
	and number2 = please_recognize (check (Union [int_l])) "12345"
	and reject_everything = dont_recognize (check (Union [])) "1"
	and multi_concat = please_recognize (check (Concat [j_l; stewart; will])) "JoelStewartwilliamson"
	and hyphenated1 = please_recognize (check (hyphen_name ('m',"ac") ('l',"aughlin"))) "Mac-laughlin"
	and hyphenated2 = please_recognize (check (hyphen_name ('m',"ac") ('l',"aughlin"))) "Mac-Laughlin"
	and over_broad = please_recognize (check (Star Wildcard)) "OMcKeefe"
	and greek = please_recognize (check (Star Wildcard)) "ηξκδησκξ"
	and long_joel = please_recognize (check (Concat [(name 'j' "oel");(Maybe (Concat [String " ";stewart]));String " ";will])) "Joel Williamson"
	and not_all_nums = dont_recognize (check (Star (Class Char.is_digit))) "123a"
	and two_words = dont_recognize (check (Concat [Star Wildcard;String " ";Star Wildcard])) "ghksakjghkja"
	and prefix_positive _ = assert_bool "Prefix positive" (longest_matching_prefix' jg_m "Gwen's name" = Some "Gwen")
	and prefix_negative _ = assert_bool "Prefix negative" (longest_matching_prefix' jg_m "Joe" = None)
	and prefix_tail _ = assert_bool "Prefix tail" (longest_matching_prefix int_l "123a2" = Some "123")
	and empty_star = please_recognize (check' several_jg_m) ""
	in let test_suite = "test suite">:::[
		"uppercase joel">::uppercase_joel_test;
		"lowercase joel">::lowercase_joel_test;
		"invalid joel">::invalid_joel_test;
		"uppercase Gwen">::uppercase_gwen_test;
		"lowercase joel union">::lowercase_j_2_test;
		"misspelling">::misspelled_gwen_test;
		"kleene star">::several_jg_test;
		"kleene star 2">::several_jg_spaces;
		"wildcard 1">::simple_dot;
		"wildcard 2">::joel_trailing
		;"integer 1">::number
		;"integer 2">::number2
		;"non-recognizer">::reject_everything
		;"long form name">::multi_concat
		;"names optional spaces">::spaces_2
		;"hyphen 1">::hyphenated1
		;"hyphen 2">::hyphenated2
		;"overbroad">::over_broad
		;"Greek">::greek
		;"Spaced name">::long_joel
		;"Non-numeric">::not_all_nums
		;"Space Check">::two_words
		;"prefix_positive">::prefix_positive
		;"prefix_negative">::prefix_negative
		;"Empty star">::empty_star
		]
	in run_test_tt_main test_suite
