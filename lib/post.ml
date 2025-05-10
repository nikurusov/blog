type post_id = string

type post_metadata = {
  title : string;
  date : string;
  tags : string list;
  id : post_id;
}
[@@deriving show]

type rendered_post = { metadata : post_metadata; html : string }
