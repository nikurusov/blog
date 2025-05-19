open Lwt.Infix
open Post
module Parser = Blog__parser.Parser

let storage_directory = Filename.concat "static" "posts"
let local_storage : Post.t list ref = ref []

let ptime_to_yyyy_mm_dd t =
  let (year, month, day), _ = Ptime.to_date_time t in
  Printf.sprintf "%04d-%02d-%02d" year month day

let read_file file = Lwt_io.with_file ~mode:Lwt_io.Input file Lwt_io.read

let init () =
  let search_posts =
    let files_in_directory dir =
      Lwt_stream.to_list (Lwt_unix.files_of_directory dir)
    in

    files_in_directory storage_directory
    >|= List.filter_map
          begin
            fun f -> if Filename.check_suffix f ".md" then Some f else None
          end
  in

  let parse_posts posts_files =
    let parse_post_file post =
      let post_path = Filename.concat storage_directory post in
      read_file post_path >|= fun file_content ->
      Parser.parse_post (Post_id post, `Jekyll file_content)
    in

    Lwt_list.filter_map_p
      begin
        fun post_path ->
          Lwt.catch
            (fun () -> parse_post_file post_path >>= Lwt.return_some)
            (fun _ -> Lwt.return_none)
      end
      posts_files
  in

  let sort_by_date posts =
    List.sort (fun a b -> String.compare b.metadata.date a.metadata.date) posts
  in

  search_posts >>= parse_posts >|= sort_by_date
  >>= begin
        fun posts ->
          local_storage := posts;
          Logs_lwt.info (fun f -> f "Posts are uploaded into memory")
      end

let all_posts_metadata () =
  !local_storage |> List.map (fun p -> p.metadata) |> Lwt.return

let find_post id =
  !local_storage |> List.find_opt (fun p -> id = p.metadata.id) |> Lwt.return
