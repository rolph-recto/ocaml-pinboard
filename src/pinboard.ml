#require "core.top"
#require "core.syntax"
#require "async"
#require "cohttp.async"
#require "yojson"

open Core.Std

type api_format = | JSON | XML

let string_of_format fmt =
  match fmt with
  | JSON -> "json"
  | XML -> "xml"
;;

type request_info = {
  api_token: string;
  api_format: api_format;
}

type tag = string

type bookmark = {
  url: string;
  title: string;
  description: string;
  tags: tag list;
  toread: bool;
  shared: bool;
  time: Time.t;
  meta: string;
}

module type API = sig
  type 'a deferred

  val bookmark : request_info -> string -> (bookmark option) deferred

  val recent_bookmarks : request_info -> ?count:int ->
    tag list -> (bookmark list) deferred

  val date_num_bookmarks : request_info -> tag list ->
    ((Date.t * int) list) deferred

  (*
  val all_bookmarks : request_info -> ?offset:int -> ?count:int ->
    ?from:Time.t -> ?to:Time.t -> tag list -> (bookmark list) deferred 

  val suggest_tag : request_info -> string -> (tag list) deferred

  val all_tags : request_info -> ((tag * int) list) deferred
  *)
end

module AsyncAPI : API with type 'a deferred := 'a Async.Std.Deferred.t = struct
  open Async.Std
  module HTTP = Cohttp_async
  module JSON = Yojson.Basic

  type 'a deferred = 'a Deferred.t
  
  let call_endpoint (info : request_info) (endpoint : Uri.t)
    : string Deferred.t
  = 
    let q1 = Uri.add_query_param endpoint ("auth_token", [info.api_token]) in
    let q2 = Uri.add_query_param q1 ("format", [info.api_format |> string_of_format]) in
    HTTP.Client.get q2 >>= fun (_, body) -> body |> HTTP.Body.to_string
  ;;

  let base_url endpoint = "https://api.pinboard.in/v1/" ^ endpoint

  let bookmark_of_json (json:JSON.json) : bookmark option = 
    let url = JSON.Util.member "href" json |> JSON.Util.to_string in

    let title = JSON.Util.member "description" json |> JSON.Util.to_string in

    let description = JSON.Util.member "extended" json |> JSON.Util.to_string in

    let tags =
      JSON.Util.member "tags" json |> JSON.Util.to_string
      |> String.split ~on:' ' in

    let toread =
      JSON.Util.member "toread" json |> JSON.Util.to_string
      |> fun x -> if x = "yes" then true else false in
    
    let shared =
      JSON.Util.member "shared" json |> JSON.Util.to_string
      |> fun x -> if x = "yes" then true else false in

    let time =
      JSON.Util.member "time" json |> JSON.Util.to_string
      |> Time.of_string_abs in

    let meta = JSON.Util.member "meta" json |> JSON.Util.to_string in

    Some ({ url=url; title=title; description=description; tags=tags;
            toread=toread; shared=shared; time=time; meta=meta; })
  ;;

  let bookmarks_of_json (json:JSON.json) : (bookmark option) list =
    JSON.Util.member "posts" json
    |> JSON.Util.to_list
    |> List.map ~f:bookmark_of_json
  ;;

  let bookmark info url =
    let q1 = Uri.of_string (base_url "posts/get") in
    let q2 = Uri.add_query_params' q1 [("url", url);("meta", "yes")] in
    call_endpoint info q2 >>| fun res ->
    res |> JSON.from_string |> bookmarks_of_json
        |> List.hd |> Option.value ~default:None
  ;;
  
  let recent_bookmarks info ?(count=15) tags =
    let q1 = Uri.of_string (base_url "posts/recent") in
    let q2 = Uri.add_query_params' q1 [
      ("count", string_of_int count);
      ("tags", String.concat ~sep:" " tags)
    ] in
    call_endpoint info q2 >>| fun res ->
    res |> JSON.from_string |> bookmarks_of_json
        |> List.map ~f:(Option.value_map ~default:[] ~f:(fun x -> [x]))
        |> List.join
  ;;

  let date_num_bookmarks info tags =
    let process_date (dstr, cstr) =
      let date = Date.of_string dstr in
      let count = cstr |> JSON.Util.to_string |> int_of_string in
      (date, count)
    in
    let q1 = Uri.of_string (base_url "posts/dates") in
    let q2 = Uri.add_query_params' q1 [("tags", String.concat ~sep:" " tags)] in
    call_endpoint info q2 >>| fun res ->
    res |> JSON.from_string |> fun json ->
    JSON.Util.member "dates" json
    |> JSON.Util.to_assoc
    |> List.map ~f:process_date
  ;;

  (*
  val all_bookmarks :
    request_info -> ?offset:int -> ?count:int -> ?from:Time.t -> ?to:Time.t
    -> tag list -> (bookmark list) deferred 

  val suggest_tag : request_info -> string -> (tag list) deferred

  val all_tags : request_info -> ((tag * int) list) deferred
  *)

end ;;

