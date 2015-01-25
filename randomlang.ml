open Core.Std


type language_meta = {
    name : string;
    weight : int;
}


let main_handler languages () =
    let splitter = String.lsplit2 ~on:':' in
    let available_languages = List.filter_map
        languages
        ~f:(fun lang -> match splitter lang with
            | None as n -> n
            | Some x -> let name, weight = x in
                        Some {name; weight=(int_of_string weight)}
        )
    in
    let enabled_languages = List.filter
        available_languages
        ~f:(fun lang -> match lang.weight with
                        | 0 -> false
                        | _ -> true
        )
    in
    let languages_seq = List.fold_left
        ~f:(fun result item ->
                result @ Array.to_list (Array.create item.weight item.name)
        )
        ~init:[]
        enabled_languages
    in
    let seq_len = match (List.length languages_seq) with
                  (* Random.int below requires min value of 1 *)
                  | 0 -> 1
                  | x -> x
    in
    let language_choice = (match (List.nth languages_seq (Random.int seq_len)) with
                          | Some str -> str
                          | None -> "en_US")
    in
    Printf.printf "%s.utf8\n" language_choice


let cmd_spec = 
    let open Command.Spec in
        (* verify input parameters *)
        step ( fun next_step languages ->
                    (* The first m argument to the step callback is the next callback function in the chain. *)
                    match languages with
                    | [] -> eprintf "You must specify at least one language.\n%!";
                            exit 1
                    | _ -> next_step languages )
        +> anon (sequence ("language" %: string))


let cmd = Command.basic
    ~summary: "Select a random lang from the provided weighed sequence"
    ~readme: (fun () -> "Select a random lang from the provided weighed sequence")
    cmd_spec
    main_handler
;;


(* Initialize Random so it won't use the default seed
   self_init () initializes the generator with a random seed chosen in a
   system-dependent way. If /dev/urandom is available on the host machine,
   it is used to provide a highly random initial seed. Otherwise, a less
   random seed is computed from system parameters (current time, process IDs)
*)
Random.self_init () ;;


let () =
    Command.run
        ~version: "1.1"
        ~build_info: "by Maxim Avanov"
        cmd
;;
