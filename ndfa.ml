open Core.Std ;;
open Option.Monad_infix ;;
module StateMap = Map.Make(String) ;;

type 'a transition = ('a -> bool) * string

type 'a state = {
	label : string ;
	(* Each transition has an input character it recognizes, and if it
	 * isn't a transition to a rejecting state, it has the name of the
	 * target state. *)
	transitions : 'a transition list ;
	null_transitions : string list ;
	}

type 'a machine = {
	map : 'a state StateMap.t ; (* A map from strings to states *)
	final_state : string list ; (* Now that the machines wil incorporate
		null transitions, all accepting states can be represented by
		having a null transition to a single final state.*)
	start : string ;
	}

type 'a execution = {
	substrate : 'a machine;
	current_states : string list;	(* Since this is an NDFA, the automaton
					 * can be in several states at once. *)
	};;

let union l1 l2 = l1 @ (List.filter l2 ~f:(fun x -> List.mem l1 x |> not))

let begin_executing m =
	{substrate=m; current_states=[m.start]}

let step_execution e c =
	(* For each state in the current states, if c matches a transition we
	 * add it to the next set of states. After finding the transitions
	 * possible using a character, we also expand the state set with a null
	 * transition stage. *)
	let rec null_transition_explore found reconsider =
		let new_states = List.fold_left ~init:[] reconsider
			~f:(fun acc state_name ->
			StateMap.find e.substrate.map state_name
			|> function | None -> acc | Some state ->
			state.null_transitions @ acc)
		|> List.filter ~f:(fun x -> not (List.mem found x)) in
		if (List.is_empty new_states)
		then found
		else null_transition_explore (new_states @ found) new_states in
	let next_states =
		(* Get the list of current states, including prepratory null exploration *)
		List.map (union e.current_states (null_transition_explore e.current_states e.current_states))
			~f:(fun s -> StateMap.find e.substrate.map s)
		(* Remove any states that didn't exist. This should be a no-op *)
		|> List.filter_map ~f:Fn.id
		|> List.fold_left ~init:[] ~f:(fun acc st ->
			(List.filter ~f:(fun t -> (fst t) c) st.transitions
			|> List.map ~f:snd)@acc) in
	let null_states = null_transition_explore next_states next_states in
	{ substrate = e.substrate; current_states = union next_states null_states }

let check_string m s =
	let final_states = String.fold s ~init:(begin_executing m) ~f:step_execution in
	List.map final_states.current_states ~f:(List.mem m.final_state)
	|> (fun l -> List.mem l true)

(* Regular languages have a recursive structure. A single character is a
 * regular language. A concatentation of regular languages is a regular
 * language. A Kleene star of regular languages is regular. An alternation of
 * regular languages is regular. Nothing else is regular. We can also use this
 * structure in the construction of recognizers, provided the states have
 * unique ids.
 * Stitching together the submachines in this recursive structure requires
 * them to have null transitions.
 *)

let state_counter = ref 1_000_000 (* Note that we can only have 9_000_00 states
	* before they are no longer guaranteed to have unique IDs *)
let get_state_id () =
	state_counter := !state_counter + 1 ;
	!state_counter

let construct_state label_root transitions null_transitions =
	let id = string_of_int (get_state_id ()) in
	{ label = label_root ^ id; transitions; null_transitions }
	

let rec concatenate machines =
	let aux m1 m2 =
		{ start = m1.start;
		final_state = m2.final_state ;
		map = StateMap.fold ~init:m2.map ~f:(fun ~key ~data acc ->
			if (List.mem m1.final_state key)
			then let new_state = {
				label = data.label;
				transitions = data.transitions;
				null_transitions = m2.start :: data.null_transitions } in
				StateMap.add ~key:key ~data:new_state acc
			else StateMap.add ~key ~data acc) m1.map}
	in match machines with
	| [] -> let start = construct_state "null" [] []
		in {start = start.label; final_state = []; map = StateMap.singleton start.label start}
	| [hd] -> hd
	| hd::tl -> aux hd (concatenate tl)

let alternate machines =
	let init = construct_state "alt_init" [] (List.map ~f:(fun m -> m.start) machines)
	and merge_maps acc b = StateMap.fold b ~init:acc ~f:(fun ~key ~data acc ->
		StateMap.add acc ~key ~data) in
	{ start = init.label; final_state = List.fold machines ~init:[] ~f:(fun acc m -> m.final_state @ acc);
	map =	List.fold machines ~init:StateMap.empty ~f:(fun acc b ->
		merge_maps acc b.map)
		|> StateMap.add ~key:init.label ~data:init
	}

let string_to_machine s =
	let state_list_unconnected = String.fold s ~init:[] ~f:(fun acc c ->
		((fun x -> x = c) ,construct_state (Char.to_string c) [] []) :: acc)
		|> List.rev in
	let rec connect_states = function
		| [] -> []
		| [(_,hd)] -> [hd]
		| (_,s1)::((c2,s2)::_ as tl) -> {s1 with transitions = [c2,s2.label] }::(connect_states tl) in
	if s = ""
	then let state = construct_state "singleton" [] [] in
		{start=state.label; final_state=[state.label];
		map = StateMap.singleton state.label state }
	else let second = List.hd_exn state_list_unconnected in
	let  init = construct_state "init" [fst second, (snd second).label] [] in
		{ start = init.label;
		final_state = [(List.last_exn state_list_unconnected |> snd).label];
		map = List.fold_left (connect_states state_list_unconnected)
			~init:(StateMap.singleton init.label init)
			~f:(fun acc s -> StateMap.add acc ~key:s.label ~data:s)}

let star machine =
	{machine with map = List.fold_left machine.final_state ~init:machine.map
		~f:(fun acc state_label ->
			let prev = StateMap.find_exn machine.map state_label in
			StateMap.add acc ~key:state_label			
			~data:{prev with null_transitions = machine.start::prev.null_transitions})}

let character_class_machine p =
	let final = construct_state "final" [] [] in
	let init = construct_state "singleton" [p,final.label] [] in
	{ start = init.label; final_state = [final.label];
	map = StateMap.singleton final.label final |> StateMap.add ~key:init.label ~data:init }

type regular_language =
	| String of string
	| Union of regular_language list
	| Concat of regular_language list
	| Star of regular_language
	| Wildcard
	| Class of (char -> bool)
	| Maybe of regular_language
	| Some of regular_language

type compiled = char machine

let rec machine_of_language = function
	| String s -> string_to_machine s
	| Union l -> alternate (List.map l ~f:machine_of_language)
	| Concat l -> concatenate (List.map l ~f:machine_of_language)
	| Star l -> star (machine_of_language l)
	| Wildcard -> character_class_machine (fun _ -> true)
	| Class p -> character_class_machine p
	| Maybe l -> machine_of_language (Union [l; String ""])
	| Some l -> machine_of_language (Concat [l; Star l])

let check l s = check_string (machine_of_language l) s
let compile l = machine_of_language l
let check_c m = check_string m

let () =
	let open OUnit2 in
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
	and uppercase_gwen_test = please_recognize (check_c jg_m) "Gwen"
	and lowercase_j_2_test = please_recognize (check_c jg_m) "joel"
	and misspelled_gwen_test = dont_recognize (check_c jg_m) "Gewn"
	and several_jg_test = please_recognize (check_c several_jg_m) "JoelGwengwenjoel"
	and several_jg_spaces = dont_recognize (check_c several_jg_m) "Joel joel Gwen gwen"
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
		]
	in run_test_tt_main test_suite
