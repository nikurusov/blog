open Lwt.Infix
open Tyxml.Html

(* ^([^_]+)_([0-9]{4}-[0-9]{2}-[0-9]{2})$ *)
let re =
  Str.regexp "^\\([^_]+\\)_\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)$"

type post_metadata = { name : string; date : string; path : string }

type post = {
  metadata : post_metadata;
  content : string;
  picture : string option;
}

(* must be abstraction of metadata storage *)
(* rewrite to array, because pages can be sorted back *)
let metadata : post_metadata list ref = ref []

let metadata_state_log metadata =
  metadata
  |> List.map (fun p ->
         Printf.sprintf "{name: %s; date: %s; path: %s}" p.name p.date p.path)
  |> String.concat "\n"
  |> Printf.sprintf "Metadata state:\n%s"

let parse_filename filename =
  Logs_lwt.info (fun m ->
      m "parse_filename: %s, %s" filename
        (string_of_bool (Str.string_match re filename 0)))
  >>= fun _ ->
  if Str.string_match re filename 0 then
    try
      let name = Str.matched_group 1 filename in
      let date = Str.matched_group 2 filename in
      Lwt.return (Some (name, date, filename))
    with Not_found ->
      Logs_lwt.warn (fun m -> m "Can't parse blog name %s" filename)
      >|= fun _ -> None
  else Lwt.return_none

let rec refresh_loop () =
  let get_posts =
    (* must be abstraction of file storage *)
    Lwt_stream.to_list (Lwt_unix.files_of_directory "static") >>= fun files ->
    Logs_lwt.info (fun m -> m "Files: %s" (String.concat " , " files))
    >>= fun _ ->
    Lwt_list.fold_left_s
      (fun acc file ->
        parse_filename file >|= function
        | Some info -> info :: acc
        | None -> acc)
      [] files
  in

  let refresh_metadata =
    get_posts >|= fun new_posts ->
    let converted =
      List.map (fun (name, date, path) -> { name; date; path }) new_posts
    in
    let sorted =
      List.sort (fun p1 p2 -> String.compare p2.date p1.date) converted
    in
    metadata := sorted
  in

  Lwt.catch
    (fun () ->
      refresh_metadata >>= fun _ ->
      Logs_lwt.info (fun m ->
          m "Posts metadata is updated: %s" (metadata_state_log !metadata)))
    (fun exn ->
      Logs_lwt.err (fun m ->
          m "refresh_metadata failed: %s" (Printexc.to_string exn)))
  >>= fun () -> Lwt_unix.sleep 5.0 >>= refresh_loop

let get_post post_metadata =
  let root = Filename.concat "static" post_metadata.path in

  let get_post_content =
    let path = Filename.concat root "post.md" in
    Lwt.catch
      (fun () ->
        Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read >|= fun content ->
        Some content)
      (fun _exn ->
        Logs_lwt.warn (fun m ->
            m "Failed to get post, file does not exist: %s" path)
        >|= fun () -> None)
  in

  let get_picture_path =
    (* let names = *)
    (*   List.map (fun ext -> "picture." ^ ext) [ "jpg"; "jpeg"; "png" ] *)
    (* in *)
    (* let path = Filename.concat root "picture.jpg" in *)
    (* Lwt_unix.files_of_directory root >|= fun exists -> *)
    (* if exists then Some path else None *)
    let path = Filename.concat root "picture.jpeg" in
    Lwt_unix.file_exists path >|= fun exists ->
    if exists then Some ("/" ^ path) else None
  in

  Lwt.both get_post_content get_picture_path >|= fun (content_opt, picture) ->
  Option.map
    (fun content -> { metadata = post_metadata; content; picture })
    content_opt

type rendered_post = {
  name : string;
  date : string;
  content : string;
  picture : string option;
  id : string;
}

let render_post post =
  try
    Some
      {
        name = post.metadata.name;
        date = post.metadata.date;
        content = post.content |> Omd.of_string |> Omd.to_html;
        picture = post.picture;
        id = post.metadata.path;
      }
  with _ ->
    Logs.warn (fun m ->
        m "Failed to parse markdown for post: %s" post.metadata.path);
    None

let get_posts_route request =
  let posts_count =
    Dream.query request "count"
    |> Option.map int_of_string |> Option.value ~default:5
  in
  let page =
    Dream.query request "page" |> Option.map int_of_string
    |> Option.value ~default:1
  in

  let get_posts_internal =
    let get_rendered_post post_metadata =
      get_post post_metadata >|= function
      | Some post -> render_post post
      | None -> None
    in
    !metadata
    |> List.drop ((page - 1) * posts_count)
    |> List.take posts_count
    |> Lwt_list.filter_map_s get_rendered_post
  in

  let post_link ~name ~id = a ~a:[ a_href ("/post/" ^ id) ] [ txt name ] in

  get_posts_internal >>= fun posts ->
  let posts_html =
    List.map
      (fun post ->
        div
          [
            h2 [ post_link ~name:post.name ~id:post.id ];
            p [ txt post.date ];
            (match post.picture with
            | Some pic -> img ~src:pic ~alt:post.name ()
            | None -> txt "");
          ])
      posts
  in

  let page_html =
    html
      (head (title (txt "Posts")) [])
      (body (h1 [ txt "Blog Posts" ] :: posts_html))
  in

  Logs_lwt.info (fun m -> m "test that logging are working") >>= fun _ ->
  Dream.html (Format.asprintf "%a" (pp ()) page_html)

[@@@ocamlformat "disable"]
let get_post request =
  (* handle exception *)
  let path = Dream.param request "id" in

  let post =
    parse_filename path >>= function
    | Some (name, date, path) ->
        get_post { name; date; path } >|= fun post ->
        Option.bind post render_post
    | None -> Lwt.return_none
  in

  let html_page body_content =
    html (head (title (txt "Head")) []) (body body_content)
  in

  let post_html post =
    [
      div
        [
          h2 [ txt post.name ];
          p [ txt post.date ];
          (match post.picture with
          | Some pic -> img ~src:pic ~alt:post.name ()
          | None -> txt "");
          Unsafe.data post.content;
        ];
    ]
  in

  let error_page = [ div [ h1 [ txt "This post doesn't exist" ] ] ] in

  let page =
    post >|= fun post ->
    Option.fold 
      ~none:(html_page error_page)
      ~some:(fun post -> post |> post_html |> html_page)
      post
  in

  page >>= fun page -> Dream.html (Format.asprintf "%a" (pp ()) page)
[@@@ocamlformat "enable"]

let () =
  let _ = Lwt.async (fun _ -> refresh_loop ()) in
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun request -> Dream.html "Hello, world!");
         Dream.get "/posts" get_posts_route;
         Dream.get "/post/:id" get_post;
         Dream.get "/static/**" (Dream.static "static");
       ]
