open Core.Std


type language_meta = {
    name : string;
    weight : int;
}

(* Initialize Random so it won't use the default seed
   self_init () initializes the generator with a random seed chosen in a
   system-dependent way. If /dev/urandom is available on the host machine,
   it is used to provide a highly random initial seed. Otherwise, a less
   random seed is computed from system parameters (current time, process IDs)
*)
Random.self_init () ;;

let () =
    let available_languages = [
        {
            name = "en_US";
            weight = 1;
        };
        {
            name = "de_DE";
            weight = 0;
        };
        {
            name = "es_ES";
            weight = 0;
        };
        {
            name = "fr_FR";
            weight = 0;
        };
    ] in
    let enabled_languages = List.filter available_languages ~f:(
        fun lang -> match lang.weight with
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
;;
