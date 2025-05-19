type 'a t = 'a Lwt.t

(* btw, need to return Db module *)
val init : unit -> unit t
val find_post : Post.id -> Post.t option t
val all_posts_metadata : unit -> Post.metadata list t
