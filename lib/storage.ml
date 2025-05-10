open Lwt.Infix
open Post

let storage_directory = Filename.concat "static" "posts"
let mem_posts_metadata : post_metadata list ref = ref []

let ptime_to_yyyy_mm_dd t =
  let (year, month, day), _ = Ptime.to_date_time t in
  Printf.sprintf "%04d-%02d-%02d" year month day

let post_content post_id =
  let path = Filename.concat storage_directory post_id in
  Lwt.catch
    (fun () ->
      Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read >|= fun content ->
      Some (post_id, content))
    (fun _exn -> Lwt.return_none)

(* === Refresh Loop === *)
let rec refresh_loop () =
  let get_posts =
    let find_posts files =
      Lwt_list.filter_p
        (fun f -> Lwt.return (Filename.check_suffix f ".md"))
        files
    in

    let parse_post (id, post) =
      try
        let jekyll = Jekyll_format.of_string_exn post in
        let fields = Jekyll_format.fields jekyll in
        let title = Jekyll_format.title_exn fields in
        let date = ptime_to_yyyy_mm_dd @@ Jekyll_format.date_exn fields in
        let tags =
          match Jekyll_format.find "tags" fields with
          | Some (`A lst) -> List.map Yaml.Util.to_string_exn lst
          | Some (`String s) ->
              String.split_on_char ',' s |> List.map String.trim
          | _ -> []
        in
        Lwt.return_some { title; date; tags; id }
      with _ -> Lwt.return_none
    in

    Lwt_stream.to_list (Lwt_unix.files_of_directory storage_directory)
    >>= find_posts
    >>= Lwt_list.filter_map_p post_content
    >>= Lwt_list.filter_map_p parse_post
  in

  let refresh_metadata =
    get_posts >|= fun posts ->
    let sorted = List.sort (fun a b -> String.compare b.date a.date) posts in
    mem_posts_metadata := sorted;
    sorted
  in

  Lwt.catch
    (fun () -> refresh_metadata >|= fun _ -> ())
    (fun exn ->
      Logs_lwt.err (fun m ->
          m "refresh_metadata failed: %s" (Printexc.to_string exn)))
  >>= fun () -> Lwt_unix.sleep 5.0 >>= refresh_loop

type 'a kind = 'a Lwt.t

let init = refresh_loop ()

let find_post id =
  Lwt.return @@ List.find_opt (fun p -> p.id = id) !mem_posts_metadata

(* unimplemented for now *)
let upsert_post _ = Lwt.return_unit
