open Core.Std

type bookmark
type tag
type note

type req_status

module type API = sig
  (* parametrize the deferred type we're going to use so we can implement
   * Async and Lwt backends *)
  type 'a deferred
  
  val token : string ref

  (* calls to the Pinboard API
   * these descriptions are straight from the API page *)

  (* https://api.pinboard.in/v1/posts/get
   *
   * Returns one or more posts on a single day matching the arguments.
   * If no date or url is given, date of most recent bookmark will be used.
   *)
  val bookmark : string -> (bookmark option) deferred

  (* https://api.pinboard.in/v1/posts/recent
   *
   * Returns a list of the user's most recent posts, filtered by tag.
   *)

  val recent_bookmarks : ?count:int -> tag list
    -> (bookmark list) deferred

  (* https://api.pinboard.in/v1/posts/dates
   *
   * Returns a list of dates with the number of posts at each date.
   *)
  val date_num_bookmarks : tag list ->
    ((Date.t * int) list) deferred

  (* https://api.pinboard.in/v1/posts/all
   *
   * Returns all bookmarks in the user's account.
   *)
  val all_bookmarks : ?offset:int -> ?count:int ->
    ?fromtime:Time.t -> ?totime:Time.t -> tag list -> (bookmark list) deferred 

  (* https://api.pinboard.in/v1/posts/add
   *
   * Add a bookmark. Arguments with shaded background are required.
   * Returns true if successfully added, false otherwise
   *)
  val add_bookmark : ?replace:bool -> ?shared:bool -> ?toread:bool ->
    ?createdat:Time.t -> ?tags:(tag list) ->
    string -> string -> string -> req_status deferred

  (* https://api.pinboard.in/v1/posts/delete
   *
   * Delete a bookmark. Arguments with shaded background are required.
   *)
  val delete_bookmark : string -> req_status deferred

  (* https://api.pinboard.in/v1/posts/suggest
   *
   * Returns a list of popular tags and recommended tags for a given URL.
   * Popular tags are tags used site-wide for the url; recommended tags are
   * drawn from the user's own tags.
   *)
  val suggest_tag : string -> (tag list * tag list) deferred

  (* https://api.pinboard.in/v1/tags/get
   *
   * Returns a full list of the user's tags along with the number of times they
   * were used.
   *)
  val all_tags : unit -> ((tag * int) list) deferred

  (* https://api.pinboard.in/v1/tags/delete
   *
   * Delete an existing tag.
   *)
  val delete_tag : string -> req_status deferred

  (* https://api.pinboard.in/v1/tags/rename
   *
   * Rename an tag, or fold it in to an existing tag
   *)
  val rename_tag : string -> string -> req_status deferred

  (* https://api.pinboard.in/v1/notes/list
   *
   * Returns a list of the user's notes
   * This DOES NOT return the text of a note!
   *)
  val all_notes : unit -> (note list) deferred

  (* https://api.pinboard.in/v1/notes/ID
   *
   * Returns an individual user note. The hash property is a 20 character long
   * sha1 hash of the note text.
   *)
  val note : string -> note deferred
end

module type Web_deferred = sig
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
  val get : Uri.t -> string t
end

module Make_API : functor (W : Web_deferred) -> API with type 'a deferred := 'a W.t

module AsyncAPI : API with type 'a deferred := 'a Async.Std.Deferred.t

module LwtAPI : API with type 'a deferred := 'a Lwt.t


