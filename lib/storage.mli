open Post

type 'a kind

val init : unit kind
val find_post : post_id -> post_metadata option kind
val upsert_post : post_metadata -> unit kind
