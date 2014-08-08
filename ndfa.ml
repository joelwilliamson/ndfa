open Core.Std ;;
open Option.Monad_infix ;;
module StateMap = Map.Make(String) ;;

type 'a state = {
	label : string ;
	(* Each transition has an input character it recognizes, and if it
	 * isn't a transition to a rejecting state, it has the name of the
	 * target state. *)
	transitions : ('a * string) list ;
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

(*
let valid_state_map m =
	List.for_all (StateMap.keys m) ~f:(fun k -> 
		match (StateMap.find m k) with
		| None -> false
		| Some v -> v.label = k
		)

let valid_machine m =
	let state_names:string list = StateMap.keys m
	and states = StateMap.data m in
	let transition_targets:string list  = List.fold_left states ~init:[] ~f:(fun acc x ->
		(List.map x.transitions snd) @ acc)
	in
	(* Each transition target should also be a state name *)
	List.exists transition_targets (fun x -> List.mem state_names x)
	&& (valid_state_map m);;

let construct_machine state_list start_state final_state =
	{map=List.fold_left state_list ~init:StateMap.empty ~f:(fun acc x ->
		StateMap.add acc ~key:x.label ~data:x) ;
	start=start_state ;
	final_state = final_state }
*)

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
		List.map (union e.current_states (null_transition_explore e.current_states e.current_states))
			~f:(fun s -> StateMap.find e.substrate.map s)
		|> List.filter_map ~f:Fn.id
		|> List.fold_left ~init:[] ~f:(fun acc st ->
			(List.filter ~f:(fun t -> (fst t) = c) st.transitions
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
	

let concatenate m1 m2 =
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

let alternate a b =
	let init = construct_state "alt_init" [] [a.start;b.start] in
	{ start = init.label; final_state = a.final_state @ b.final_state;
	map =	StateMap.fold a.map ~init:b.map ~f:(fun ~key ~data acc ->
			StateMap.add acc ~key ~data)
		|> StateMap.add ~key:init.label ~data:init
	}

let string_to_machine s =
	let state_list_unconnected = String.fold s ~init:[] ~f:(fun acc c ->
		(c,construct_state (Char.to_string c) [] []) :: acc)
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

type regular_language =
	| String of string
	| Union of regular_language * regular_language
	| Concat of regular_language * regular_language
	| Star of regular_language

type compiled = char machine

let rec machine_of_language = function
	| String s -> string_to_machine s
	| Union (l1,l2) -> alternate (machine_of_language l1) (machine_of_language l2)
	| Concat (l1,l2) -> concatenate (machine_of_language l1) (machine_of_language l2)
	| Star l -> star (machine_of_language l)

let check l s = check_string (machine_of_language l) s
let compile l = machine_of_language l
let check_c m = check_string m
