open Post

let ptime_to_yyyy_mm_dd t =
  let (year, month, day), _ = Ptime.to_date_time t in
  Printf.sprintf "%04d-%02d-%02d" year month day

let parse_post = function
  | Post_id id, `Jekyll raw_post ->
      let jekyll = Jekyll_format.of_string_exn raw_post in
      let fields = Jekyll_format.fields jekyll in
      let title = Jekyll_format.title_exn fields in
      let date = ptime_to_yyyy_mm_dd @@ Jekyll_format.date_exn fields in
      let tags =
        match Jekyll_format.find "tags" fields with
        | Some (`A lst) -> List.map Yaml.Util.to_string_exn lst
        | Some (`String s) -> String.split_on_char ',' s |> List.map String.trim
        | _ -> []
      in
      let content = Jekyll_format.body jekyll in
      {
        content = `Html content;
        metadata = { title; date; tags; id = Post_id id };
      }
