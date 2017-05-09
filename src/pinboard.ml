(* pinboard.ml *)

(* uncomment for utop
#require "core.top"
#require "core.syntax"
#require "async"
#require "cohttp.async"
#require "yojson"
*)

open Core.Std

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

  val token : string ref

  val bookmark : string -> (bookmark option) deferred

  val recent_bookmarks : ?count:int ->
    tag list -> (bookmark list) deferred

  val date_num_bookmarks : tag list ->
    ((Date.t * int) list) deferred

  val all_bookmarks : ?offset:int -> ?count:int ->
    ?fromtime:Time.t -> ?totime:Time.t -> tag list -> (bookmark list) deferred 

  val suggest_tag : string -> (tag list * tag list) deferred

  val all_tags : unit -> ((tag * int) list) deferred
end

module AsyncAPI : API with type 'a deferred := 'a Async.Std.Deferred.t = struct
  open Async.Std
  module HTTP = Cohttp_async
  module JSON = Yojson.Basic

  type 'a deferred = 'a Deferred.t

  let token = ref ""
  
  let call_endpoint (endpoint : Uri.t) : string Deferred.t
  = 
    let q1 = Uri.add_query_param' endpoint ("auth_token", !token) in
    let q2 = Uri.add_query_param' q1 ("format", "json") in
    HTTP.Client.get q2 >>= fun (_, body) ->
    body |> HTTP.Body.to_string
  ;;

  let base_url endpoint = "https://api.pinboard.in/v1/" ^ endpoint

  let bookmark_of_json (json:JSON.json) : bookmark = 
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

    { url=url; title=title; description=description; tags=tags;
      toread=toread; shared=shared; time=time; meta=meta; }
  ;;

  let bookmarks_of_json (json:JSON.json) : bookmark list =
    JSON.Util.member "posts" json
    |> JSON.Util.to_list
    |> List.map ~f:bookmark_of_json
  ;;

  let bookmark url =
    let q1 = Uri.of_string (base_url "posts/get") in
    let q2 = Uri.add_query_params' q1 [("url", url);("meta", "yes")] in
    call_endpoint q2 >>| fun res ->
    res |> JSON.from_string |> bookmarks_of_json |> List.hd
  ;;
  
  let recent_bookmarks ?(count=15) tags =
    let q1 = Uri.of_string (base_url "posts/recent") in
    let q2 = Uri.add_query_params' q1 [
      ("count", string_of_int count);
      ("tags", String.concat ~sep:" " tags)
    ] in
    call_endpoint q2 >>| fun res ->
    res |> JSON.from_string |> bookmarks_of_json
  ;;

  let date_num_bookmarks tags =
    let process_date (dstr, cstr) =
      let date = Date.of_string dstr in
      let count = cstr |> JSON.Util.to_string |> int_of_string in
      (date, count)
    in
    let q1 = Uri.of_string (base_url "posts/dates") in
    let q2 = Uri.add_query_params' q1 [("tags", String.concat ~sep:" " tags)] in
    call_endpoint q2 >>| fun res ->
    res |> JSON.from_string |> fun json ->
    JSON.Util.member "dates" json
    |> JSON.Util.to_assoc
    |> List.map ~f:process_date
  ;;

  let all_bookmarks ?(offset=0) ?count ?fromtime ?totime tags =
    let params = 
      [("start", string_of_int offset)]
      |> (fun q -> Option.value_map count ~default:q
                   ~f:(fun n -> ("results", string_of_int n)::q))
      |> (fun q -> Option.value_map fromtime ~default:q
                   ~f:(fun dt -> ("fromdt", Time.to_string_abs dt)::q))
      |> (fun q -> Option.value_map totime ~default:q
                   ~f:(fun dt -> ("todt", Time.to_string_abs dt)::q))
      |> (fun q -> ("tags", String.concat ~sep:" " tags)::q)
    in
    let q1 = Uri.of_string (base_url "posts/all") in
    let q2 = Uri.add_query_params' q1 params in
    call_endpoint q2 >>| fun res ->
    res |> JSON.from_string |> JSON.Util.to_list |> List.map ~f:bookmark_of_json
  ;;

  let suggest_tag url =
    let q1 = Uri.of_string (base_url "posts/suggest") in
    let q2 = Uri.add_query_param' q1 ("url", url) in
    call_endpoint q2 >>| fun res ->
    res |> JSON.from_string |> JSON.Util.to_list
        |> List.map ~f:JSON.Util.to_assoc |> fun lst ->
    let mem_filter mem lst =
      List.filter ~f:(fun map -> List.Assoc.find map mem |> Option.is_some) lst
      |> List.hd |> Option.value ~default:[] 
    in
    let get_mem mem map =
      let stringify x =
        JSON.Util.to_list x |> List.map ~f:JSON.Util.to_string
      in
      List.Assoc.find map mem |> Option.value_map ~default:[] ~f:stringify
    in
    let popular = lst |> mem_filter "popular" |> get_mem "popular" in
    let recommended = lst |> mem_filter "recommended" |> get_mem "recommended" in
    (popular, recommended)
  ;;

  let all_tags () =
    let q1 = Uri.of_string (base_url "tags/get") in
    call_endpoint q1 >>| fun res ->
    res |> JSON.from_string |> JSON.Util.to_assoc
        |> List.map ~f:(fun (tag, n) -> (tag, n |> JSON.Util.to_string |> int_of_string))
  ;;

end

