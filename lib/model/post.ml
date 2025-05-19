type id = Post_id of string

type metadata = { title : string; date : string; tags : string list; id : id }
[@@deriving show]

type t = { metadata : metadata; content : [ `Html of string ] }
