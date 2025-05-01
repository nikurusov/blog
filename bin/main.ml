open Lwt.Infix
open Tyxml.Html

let storage_directory = Filename.concat "static" "storage"
let picture_file_name = "picture"
let post_file_name = "post"

let post_name_regex =
  Str.regexp "^\\([^_]+\\)_\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)$"

type post_metadata = { name : string; date : string; id : string }
[@@deriving show]

type post_info = { metadata : post_metadata; picture_path : string option }
[@@deriving show]

type post = { info : post_info; raw_content : string } [@@deriving show]

(* must be abstraction of metadata storage *)
(* rewrite to array, because pages can be sorted back *)
let mem_posts_info : post_info list ref = ref []

let log_file_parsing filename result =
  Logs_lwt.info (fun m ->
      m "Parsing: filename - %s, result - %s" filename result)

let log_files_in_storage files =
  Logs_lwt.info (fun m -> m "Files: %s" (String.concat " , " files))
  >|= fun _ -> files

[@@@ocamlformat "disable"]
let log_refresh_memory_posts_info (posts_info : post_info list) =
  let log =
    posts_info
    |> List.map show_post_info
    |> String.concat ", "
    |> Printf.sprintf "[ %s ]"
  in
  Logs_lwt.info (fun m -> m "Posts metadata is updated: %s" log)
[@@@ocamlformat "enable"]

(* convert yyyy-mm-dd to human format, ex: 6 September 2025 *)
let human_readable_date str =
  match String.split_on_char '-' str with
  | [ year; month; day ] ->
      let month_names =
        [|
          "January";
          "February";
          "March";
          "April";
          "May";
          "June";
          "July";
          "August";
          "September";
          "October";
          "November";
          "December";
        |]
      in
      let month_num = int_of_string month in
      let day_num = int_of_string day in
      let year_num = int_of_string year in
      Printf.sprintf "%d %s %d" day_num month_names.(month_num - 1) year_num
  | _ ->
      Printf.eprintf "Invalid date format: %s\n" str;
      str

let parse_filename filename =
  let is_post = Str.string_match post_name_regex filename 0 in
  log_file_parsing filename (string_of_bool is_post) >>= fun _ ->
  if is_post then
    try
      let name = Str.matched_group 1 filename in
      let date = human_readable_date @@ Str.matched_group 2 filename in
      Lwt.return (Some (name, date, filename))
    with Not_found | Failure _ ->
      Logs_lwt.warn (fun m -> m "Can't parse blog name %s" filename)
      >|= fun _ -> None
  else Lwt.return_none

let markdown_to_html md = md |> Omd.of_string |> Omd.to_html

let rec refresh_loop () =
  let get_post_picture post_directory =
    let base_name = Filename.concat post_directory "picture" in
    let candidates =
      List.map (fun ext -> base_name ^ ext) [ ".jpeg"; ".png"; ".jpg" ]
    in
    Lwt.catch
      (fun () ->
        Lwt_list.find_s Lwt_unix.file_exists candidates >|= fun path_opt ->
        Some path_opt)
      (fun _exn ->
        Logs_lwt.warn (fun m ->
            m "No picture found for post in: %s" post_directory)
        >|= fun () -> None)
  in
  let get_post_metadata file =
    let to_metadata opt =
      Option.map (fun (name, date, path) -> { name; date; id = path }) opt
    in
    parse_filename file >|= to_metadata
  in
  let get_posts =
    let find_posts files =
      Lwt_list.fold_left_s
        (fun acc file ->
          get_post_metadata file >>= function
          | Some metadata ->
              get_post_picture (Filename.concat storage_directory metadata.id)
              >|= fun picture_path -> { metadata; picture_path } :: acc
          | None -> Lwt.return acc)
        [] files
    in

    (* must be abstraction of file storage *)
    Lwt_stream.to_list (Lwt_unix.files_of_directory storage_directory)
    >>= log_files_in_storage >>= find_posts
  in

  let refresh_metadata =
    get_posts >|= fun new_posts ->
    let sorted =
      List.sort
        (fun p1 p2 -> String.compare p2.metadata.date p1.metadata.date)
        new_posts
    in
    let () = mem_posts_info := sorted in
    sorted
  in

  Lwt.catch
    (fun () -> refresh_metadata >>= log_refresh_memory_posts_info)
    (fun exn ->
      Logs_lwt.err (fun m ->
          m "refresh_metadata failed: %s" (Printexc.to_string exn)))
  >>= fun () -> Lwt_unix.sleep 5.0 >>= refresh_loop

let get_post_with_content post_info =
  let root_file = Filename.concat storage_directory post_info.metadata.id in

  (* Read post.md content *)
  let get_post_content =
    let path = Filename.concat root_file "post.md" in
    Lwt.catch
      (fun () ->
        Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read >|= fun content ->
        Some content)
      (fun _exn ->
        Logs_lwt.warn (fun m ->
            m "Failed to read post content: file does not exist (%s)" path)
        >|= fun () -> None)
  in

  get_post_content >|= fun content_opt ->
  Option.map
    (fun content -> { info = post_info; raw_content = content })
    content_opt

type rendered_post = {
  name : string;
  date : string;
  content : string;
  picture_path : string option;
  id : string;
}

let make_uri_absolute picture_uri = "/" ^ picture_uri

let render_post post =
  try
    Some
      {
        name = post.info.metadata.name;
        date = post.info.metadata.date;
        content = markdown_to_html post.raw_content;
        picture_path = post.info.picture_path;
        id = post.info.metadata.id;
      }
  with _ ->
    Logs.warn (fun m ->
        m "Failed to parse markdown for post: %s" post.info.metadata.id);
    None

let site_navbar ~active =
  let nav_link name path =
    let cls = if active = name then [ a_class [ "active" ] ] else [] in
    li [ a ~a:(a_href path :: cls) [ txt name ] ]
  in
  nav
    ~a:[ a_class [ "site-navbar" ] ]
    [
      ul
        [
          nav_link "About" "/";
          nav_link "Blog" "/posts";
          nav_link "Contact" "/contact";
        ];
    ]

let common_html_links =
  [
    link ~rel:[ `Stylesheet ]
      ~href:"https://fonts.googleapis.com/css2?family=Merriweather&display=swap"
      ();
    link ~rel:[ `Stylesheet ] ~href:"/static/style.css" ();
  ]

let get_posts_route request =
  let posts_count =
    Dream.query request "count"
    |> Option.map int_of_string |> Option.value ~default:5
  in
  let page =
    Dream.query request "page" |> Option.map int_of_string
    |> Option.value ~default:1
  in
  let posts_preview =
    !mem_posts_info
    |> List.drop ((page - 1) * posts_count)
    |> List.take posts_count
  in

  let post_link ~name ~id = a ~a:[ a_href ("/post/" ^ id) ] [ txt name ] in

  let posts_html =
    ul
      (List.map
         (fun post ->
           li
             [
               div
                 ~a:[ a_class [ "post-line" ] ]
                 [
                   span
                     ~a:[ a_class [ "post-date" ] ]
                     [ txt (human_readable_date post.metadata.date) ];
                   h2
                     [ post_link ~name:post.metadata.name ~id:post.metadata.id ];
                 ];
             ])
         posts_preview)
  in

  let head_html =
    head
      (title (txt "Posts"))
      (common_html_links
      @ [ link ~rel:[ `Stylesheet ] ~href:"/static/posts.css" () ])
  in

  let body_html =
    body
      [
        site_navbar ~active:"Blog";
        div ~a:[ a_class [ "container" ] ] (h1 [ txt "Blog" ] :: [ posts_html ]);
      ]
  in

  let page_html = html head_html body_html in

  Logs_lwt.info (fun m -> m "test that logging are working") >>= fun _ ->
  Dream.html (Format.asprintf "%a" (pp ()) page_html)

[@@@ocamlformat "disable"]
let get_post_route request =
  (* handle exception *)
  let id = Dream.param request "id" in

  let post =
    match List.find_opt (fun info -> info.metadata.id = id) !mem_posts_info with
    | Some info ->
        get_post_with_content info >|= fun post ->
        Option.bind post render_post
    | None ->
        Lwt.return_none
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
          (match post.picture_path with
          | Some pic -> img ~src:(make_uri_absolute pic) ~alt:post.name ()
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

let on_none_not_found_response (opt : 'a option)
    (f : 'a -> Dream.response Lwt.t) : Dream.response Lwt.t =
  match opt with
  | Some x -> f x
  | None -> Dream.html ~status:`Not_Found "<h1>404 Not Found</h1>"

let get_about_route _request =
  let get_about_md =
    let path = Filename.concat "static" "about.md" in
    Lwt.catch
      (fun () ->
        Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read >|= fun content ->
        Some content)
      (fun _exn ->
        Logs_lwt.warn (fun m ->
            m "Failed to read 'about.md': file does not exist (%s)" path)
        >|= fun () -> None)
  in

  get_about_md >>= fun opt ->
  on_none_not_found_response opt (fun about_md ->
      let about_html = markdown_to_html about_md in
      let page =
        html
          (head
             (title (txt "About"))
             (common_html_links
             @ [ link ~rel:[ `Stylesheet ] ~href:"/static/about.css" () ]))
          (body
             [
               site_navbar ~active:"About";
               div
                 ~a:[ a_class [ "container" ] ]
                 [ h1 [ txt "About" ]; Unsafe.data about_html ];
             ])
      in
      Dream.html (Format.asprintf "%a" (pp ()) page))

let get_contact_route _req = Dream.html "<h1>Contact page coming soon.</h1>"

let () =
  let _ = Lwt.async (fun _ -> refresh_loop ()) in
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" get_about_route;
         Dream.get "/posts" get_posts_route;
         Dream.get "/post/:id" get_post_route;
         Dream.get "/contact" get_contact_route;
         Dream.get "/static/**" (Dream.static "static");
       ]
