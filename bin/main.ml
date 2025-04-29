open Lwt.Infix

let re = Str.regexp "^\\([^_]+\\)_\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)$"

type post_metadata = { name : string; date : string; path : string }

(* must be abstraction of metadata storage *)
(* rewrite to array, because pages can be sorted back *)
let metadata : post_metadata list ref = ref []

let parse_filename filename =
  if Str.string_match re filename 0 then
    try
      let name = Str.matched_group 1 filename in
      let date = Str.matched_group 2 filename in
      Lwt.return (Some (name, date, filename))
    with Not_found ->
      Logs_lwt.warn (fun m -> m "Can't parse blog with name %s" filename)
      >|= fun _ -> None
  else Lwt.return_none

let get_posts =
  (* must be abstraction of file storage *)
  Lwt_stream.to_list (Lwt_unix.files_of_directory "static") >>= fun files ->
  Lwt_list.fold_left_s
    (fun acc file ->
      parse_filename file >|= function Some info -> info :: acc | None -> acc)
    [] files

let refresh_metadata =
  get_posts >|= fun new_posts ->
  let converted =
    List.map (fun (name, date, path) -> { name; date; path }) new_posts
  in
  let sorted =
    List.sort (fun p1 p2 -> String.compare p2.date p1.date) converted
  in
  metadata := sorted

let () =
  Dream.run @@ Dream.logger
  @@ Dream.router [ Dream.get "/" (fun _ -> Dream.html "Hello, world!") ]
