open Lwt.Infix

let storage_directory = Filename.concat "static" "posts"

(* === Types === *)
type post_metadata = {
  title : string;
  date : string;
  tags : string list;
  id : string;
}
[@@deriving show]

type rendered_post = { metadata : post_metadata; html : string }

(* === In-Memory Storage === *)
let mem_posts_metadata : post_metadata list ref = ref []

(* === Utilities === *)
let ptime_to_yyyy_mm_dd t =
  let (year, month, day), _ = Ptime.to_date_time t in
  Printf.sprintf "%04d-%02d-%02d" year month day

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
  | _ -> str

let markdown_to_html md = md |> Omd.of_string |> Omd.to_html

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

(* === Rendering === *)
let render_post metadata raw_jekyll =
  try
    let jekyll = Jekyll_format.of_string_exn raw_jekyll in
    let markdown = Jekyll_format.body jekyll in
    Some { metadata; html = markdown_to_html markdown }
  with _ -> None

(* === HTML === *)
open Tyxml.Html

let site_navbar ~active =
  let nav_link name path =
    let cls = if active = name then [ a_class [ "active" ] ] else [] in
    li [ a ~a:(a_href path :: cls) [ txt name ] ]
  in
  nav
    ~a:[ a_class [ "site-navbar" ] ]
    [ ul [ nav_link "About" "/"; nav_link "Blog" "/posts" ] ]

let common_html_links =
  [
    link ~rel:[ `Stylesheet ]
      ~href:"https://fonts.googleapis.com/css2?family=Merriweather&display=swap"
      ();
    link ~rel:[ `Stylesheet ] ~href:"/static/style.css" ();
  ]

let get_posts_route _request =
  let posts_preview = !mem_posts_metadata in

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
                     [ txt (human_readable_date post.date) ];
                   span ~a:[ a_class [ "separator" ] ] [ txt "|" ];
                   post_link ~name:post.title ~id:post.id;
                 ];
             ])
         posts_preview)
  in

  let page_html =
    html
      (head (title (txt "Posts")) common_html_links)
      (body
         [
           site_navbar ~active:"Blog";
           div ~a:[ a_class [ "container" ] ] [ h1 [ txt "Blog" ]; posts_html ];
         ])
  in
  Dream.html (Format.asprintf "%a" (pp ()) page_html)

let get_post_route request =
  let id = Dream.param request "id" in
  match List.find_opt (fun m -> m.id = id) !mem_posts_metadata with
  | Some metadata -> (
      post_content metadata.id >>= function
      | Some (_, raw) -> (
          match render_post metadata raw with
          | Some post ->
              let page =
                html
                  (head (title (txt post.metadata.title)) common_html_links)
                  (body
                     [
                       site_navbar ~active:"Blog";
                       div
                         ~a:[ a_class [ "container" ] ]
                         [
                           h2 [ txt post.metadata.title ];
                           p [ txt post.metadata.date ];
                           Unsafe.data post.html;
                         ];
                     ])
              in
              Dream.html (Format.asprintf "%a" (pp ()) page)
          | None -> Dream.html ~status:`Not_Found "<h1>Render error</h1>")
      | None -> Dream.html ~status:`Not_Found "<h1>Post not found</h1>")
  | None -> Dream.html ~status:`Not_Found "<h1>Post not found</h1>"

let get_about_route _request =
  let path = Filename.concat "static" "about.md" in
  Lwt.catch
    (fun () ->
      Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read >>= fun content ->
      let about_html = markdown_to_html content in
      let page =
        html
          (head (title (txt "About")) common_html_links)
          (body
             [
               site_navbar ~active:"About";
               div ~a:[ a_class [ "container" ] ] [ Unsafe.data about_html ];
             ])
      in
      Dream.html (Format.asprintf "%a" (pp ()) page))
    (fun _ -> Dream.html ~status:`Not_Found "<h1>About page not found</h1>")

let () =
  let _ = Lwt.async (fun _ -> refresh_loop ()) in
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" get_about_route;
         Dream.get "/posts" get_posts_route;
         Dream.get "/post/:id" get_post_route;
         Dream.get "/static/**" (Dream.static "static");
       ]
