open Core.Std ;;
open Option.Monad_infix ;;
module StateMap = Map.Make(String) ;;

type 'a transition =
	| Fun of ('a -> bool) * string
	| Single of 'a * string

let check_transition (c:'a) (t:'a transition) =
	match t with
	| Fun (t,_) -> t c
	| Single (t,_) -> t = c
let transition_target = function
	| Fun (_,t) -> t
	| Single (_,t) -> t

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

let get_state m state_name = StateMap.find m.map state_name

(* This is O(n^2)! Even sorting would be faster *)
(* The asymptotic behaviour doesn't seem to matter. union currently takes
 * about 4% of execution time in the c_lang benchmark *)
let union l1 l2 = l1 @ (List.filter l2 ~f:(fun x -> List.mem l1 x |> not))
(* For each state in the current states, if c matches a transition we
 * add it to the next set of states. After finding the transitions
 * possible using a character, we also expand the state set with a null
 * transition stage. *)
let rec null_transition_explore m found reconsider =
	let new_states = List.fold_left ~init:[] reconsider
		~f:(fun acc state_name ->
			get_state m state_name
			|> function | None -> acc | Some state ->
				state.null_transitions @ acc)
		|> List.filter ~f:(fun x -> not (List.mem found x)) in
	if (List.is_empty new_states)
	then found
	else null_transition_explore m (new_states @ found) new_states

let begin_executing m =
	{substrate=m; current_states=m.start:: (null_transition_explore m [m.start] [m.start])}



let step_execution e c =
	let next_states = List.map e.current_states
			~f:(fun s -> get_state e.substrate s)
			(* O(NlgN) *)
		(* Remove any states that didn't exist. This should be a no-op *)
		|> List.filter_map ~f:Fn.id
			(* O(N) *)
		|> List.fold_left ~init:[] ~f:(fun acc st ->
			(List.filter ~f:(check_transition c) st.transitions
			|> List.map ~f:transition_target)@acc) in
	let null_states = null_transition_explore e.substrate next_states next_states in
	{ substrate = e.substrate; current_states = union next_states null_states }

(** Is the context in a valid final state? **)
let matched e =
	List.map e.current_states ~f:(List.mem e.substrate.final_state)
	|> List.exists ~f:Fn.id

let check_string m s =
	let final_states = String.fold s ~init:(begin_executing m) ~f:step_execution in
	matched final_states

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

let construct_state label_root transitions null_transitions :'a state=
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
		(c ,construct_state (Char.to_string c) [] []) :: acc)
		|> List.rev in
	let rec connect_states = function
		| [] -> []
		| [(_,hd)] -> [hd]
		| (_,s1)::((c2,s2)::_ as tl) ->
			{s1 with transitions = [Single (c2,s2.label)] }::(connect_states tl) in
	if s = ""
	then let state = construct_state "singleton" [] [] in
		{start=state.label; final_state=[state.label];
		map = StateMap.singleton state.label state }
	else let second = List.hd_exn state_list_unconnected in
	let  init = construct_state "init" [Single (fst second, (snd second).label)] [] in
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
			~data:{prev with null_transitions = machine.start::prev.null_transitions});
		final_state = machine.start::machine.final_state}

let character_class_machine p =
	let final = construct_state "final" [] [] in
	let init = construct_state "singleton" [Fun (p,final.label)] [] in
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
	| Several of regular_language

type compiled = char machine
type exec_context = char execution

let rec machine_of_language = function
	| String s -> string_to_machine s
	| Union l -> alternate (List.map l ~f:machine_of_language)
	| Concat l -> concatenate (List.map l ~f:machine_of_language)
	| Star l -> star (machine_of_language l)
	| Wildcard -> character_class_machine (fun _ -> true)
	| Class p -> character_class_machine p
	| Maybe l -> machine_of_language (Union [l; String ""])
	| Several l -> machine_of_language (Concat [l; Star l])

let check l s = check_string (machine_of_language l) s
let compile l = machine_of_language l
let check' m = check_string m

let get_char_checker l = machine_of_language l |> begin_executing
let get_char_checker' m = begin_executing m

let check_char e c =
	let result = step_execution e c
	in (result, List.is_empty result.current_states |> not,matched result) 

let longest_matching_prefix' m s =
	String.fold ~init:((get_char_checker' m),None,"") ~f:(fun (e,longest,interim) c ->
		let (e',maybe,matched) = check_char e c in
		if not maybe then (e',longest,interim)
		else if matched then match longest with
			| None -> e',Some (interim ^ (String.of_char c)),""
			| Some s -> e',Some (s ^ interim ^ (String.of_char c)),""
		else e',longest,interim ^ (String.of_char c)) s
	|> function | (_,longest,_) -> longest

let longest_matching_prefix (l:regular_language)  =
	longest_matching_prefix' (machine_of_language l)
