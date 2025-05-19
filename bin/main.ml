open Lwt.Infix
open Post

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
  let post_link ~name ~id = a ~a:[ a_href ("/posts/" ^ id) ] [ txt name ] in

  let posts_html (posts_preview : Post.metadata list) =
    ul
      (List.map
         (fun post ->
           let (Post_id raw_id) = post.id in
           print_endline (Printf.sprintf "post uri %s" raw_id);
           li
             [
               div
                 ~a:[ a_class [ "post-line" ] ]
                 [
                   span
                     ~a:[ a_class [ "post-date" ] ]
                     [ txt (human_readable_date post.date) ];
                   span ~a:[ a_class [ "separator" ] ] [ txt "|" ];
                   post_link ~name:post.title ~id:raw_id;
                 ];
             ])
         posts_preview)
  in

  let page_html previews =
    html
      (head (title (txt "Posts")) common_html_links)
      (body
         [
           site_navbar ~active:"Blog";
           div
             ~a:[ a_class [ "container" ] ]
             [ h1 [ txt "Blog" ]; posts_html previews ];
         ])
  in

  Blog.Db.all_posts_metadata () >>= fun previews ->
  Dream.html (Format.asprintf "%a" (pp ()) (page_html previews))

let get_post_route request =
  let id = Dream.param request "id" in
  Blog.Db.find_post (Post_id id) >>= function
  | Some { metadata; content = `Html raw_content } ->
      let page =
        html
          (head (title (txt metadata.title)) common_html_links)
          (body
             [
               site_navbar ~active:"Blog";
               div
                 ~a:[ a_class [ "container" ] ]
                 [
                   div
                     ~a:[ a_class [ "post-header" ] ]
                     [
                       span
                         ~a:[ a_class [ "post-date" ] ]
                         [ txt (human_readable_date metadata.date) ];
                       h1 ~a:[ a_class [ "post-title" ] ] [ txt metadata.title ];
                     ];
                   Unsafe.data (markdown_to_html raw_content);
                 ];
             ])
      in
      Dream.html (Format.asprintf "%a" (pp ()) page)
  | None -> Dream.html ~status:`Not_Found "<h1>Render error</h1>"

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
  Lwt_main.run
    begin
      Blog.Db.init () >>= fun () ->
      Dream.serve @@ Dream.logger
      @@ Dream.router
           [
             Dream.get "/" get_about_route;
             Dream.get "/posts" get_posts_route;
             Dream.get "/posts/:id" get_post_route;
             Dream.get "/static/**" (Dream.static "static");
           ]
    end
