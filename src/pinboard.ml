(* pinboard.ml *)

#require "core.top"
#require "core.syntax"
#require "yojson"
#require "async"
#require "cohttp.async"
#require "lwt"
#require "cohttp.lwt"

open Core.Std
module JSON = Yojson.Basic

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

type note = {
  id: string;
  hash: string;
  title: string;
  created_at: Time.t;
  updated_at: Time.t;
  text: string;
}

type req_status = | Success | Err of string

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

  val add_bookmark : ?replace:bool -> ?shared:bool -> ?toread:bool ->
    ?createdat:Time.t -> ?tags:(tag list) ->
    string -> string -> string -> req_status deferred

  val delete_bookmark : string -> req_status deferred

  val suggest_tag : string -> (tag list * tag list) deferred

  val all_tags : unit -> ((tag * int) list) deferred

  val delete_tag : string -> req_status deferred

  val rename_tag : string -> string -> req_status deferred

  val all_notes : unit -> (note list) deferred

  val note : string -> note deferred
end

module type Web_deferred = sig
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
  val get : Uri.t -> string t
end

module Make_API (W : Web_deferred)
  : API with type 'a deferred = 'a W.t
= struct
  type 'a deferred = 'a W.t

  let token = ref ""
  
  let call_endpoint (endpoint : Uri.t) : string deferred
  = 
    let q1 = Uri.add_query_param' endpoint ("auth_token", !token) in
    let q2 = Uri.add_query_param' q1 ("format", "json") in
    W.get q2
  ;;

  let base_url endpoint = "https://api.pinboard.in/v1/" ^ endpoint

  let bookmark_of_json (json:JSON.json) : bookmark = 
    let open W in
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

    { url; title; description; tags; toread; shared; time; meta; }
  ;;

  let bookmarks_of_json (json:JSON.json) : bookmark list =
    let open W in
    JSON.Util.member "posts" json
    |> JSON.Util.to_list
    |> List.map ~f:bookmark_of_json
  ;;

  let bookmark url =
    let open W in
    let q1 = Uri.of_string (base_url "posts/get") in
    let q2 = Uri.add_query_params' q1 [("url", url);("meta", "yes")] in
    call_endpoint q2 >>| fun res ->
    res |> JSON.from_string |> bookmarks_of_json |> List.hd
  ;;
  
  let recent_bookmarks ?(count=15) tags =
    let open W in
    let q1 = Uri.of_string (base_url "posts/recent") in
    let q2 = Uri.add_query_params' q1 [
      ("count", string_of_int count);
      ("tags", String.concat ~sep:" " tags)
    ] in
    call_endpoint q2 >>| fun res ->
    res |> JSON.from_string |> bookmarks_of_json
  ;;

  let date_num_bookmarks tags =
    let open W in
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
    let open W in
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

  let parse_req_response (field:string) (res:string) : req_status =
    res |> JSON.from_string |> JSON.Util.member field
    |> JSON.Util.to_string |> fun code ->
    if code = "done" then Success else Err(code)
  ;;

  let add_bookmark ?(replace=true) ?(shared=true) ?(toread=false)
    ?createdat ?(tags=[]) url title description
  =
    let open W in
    let q1 = Uri.of_string (base_url "posts/add") in
    let q2 = Uri.add_query_params' q1 [
      ("url", url);
      ("description", title);
      ("extended", description);
      ("tags", String.concat ~sep:" " tags);
      ("dt", Option.value_map createdat
                    ~default:(Time.now () |> Time.to_string)
                    ~f:Time.to_string);
      ("replace", if replace then "yes" else "no");
      ("shared", if shared then "yes" else "no");
      ("toread", if toread then "yes" else "no")
    ] in
    call_endpoint q2 >>| parse_req_response "result_code"
  ;;

  let delete_bookmark url =
    let open W in
    let q1 = Uri.of_string (base_url "posts/delete") in
    let q2 = Uri.add_query_param' q1 ("url", url) in
    call_endpoint q2 >>| parse_req_response "result_code"
  ;;

  let suggest_tag url =
    let open W in
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
    let open W in
    let q1 = Uri.of_string (base_url "tags/get") in
    let kv_to_int (tag, n) = (tag, n |> JSON.Util.to_string |> int_of_string) in
    call_endpoint q1 >>| fun res ->
    res |> JSON.from_string |> JSON.Util.to_assoc
        |> List.map ~f:kv_to_int
  ;;

  let delete_tag tag =
    let open W in
    let q1 = Uri.of_string (base_url "tags/delete") in
    let q2 = Uri.add_query_param' q1 ("tag", tag) in
    call_endpoint q2 >>| parse_req_response "result"
  ;;

  let rename_tag oldtag newtag =
    let open W in
    let q1 = Uri.of_string (base_url "tags/rename") in
    let q2 = Uri.add_query_params' q1 [("old", oldtag);("new", newtag)] in
    printf "%s\n" (q2 |> Uri.to_string);
    call_endpoint q2 >>| parse_req_response "result"
  ;;

  let note_of_json (has_text : bool) (json : JSON.json) : note =
    let open W in
    let id = JSON.Util.member "id" json |> JSON.Util.to_string in
    let hash = JSON.Util.member "hash" json |> JSON.Util.to_string in
    let title = JSON.Util.member "title" json |> JSON.Util.to_string in
    let created_at = JSON.Util.member "created_at" json |> JSON.Util.to_string
                  |> Time.of_string in
    let updated_at = JSON.Util.member "created_at" json |> JSON.Util.to_string
                  |> Time.of_string in
    let text =
      if has_text
      then JSON.Util.member "text" json |> JSON.Util.to_string
      else ""
    in
    { id; hash; title; created_at; updated_at; text }
  ;;
  
  let all_notes () =
    let open W in
    let q1 = Uri.of_string (base_url "notes/list") in
    call_endpoint q1 >>| fun res ->
    res |> JSON.from_string |> JSON.Util.to_assoc
        |> (fun dict -> List.Assoc.find dict "notes")
        |> Option.value_map ~default:[] ~f:JSON.Util.to_list
        |> List.map ~f:(note_of_json false)
  ;;

  let note id =
    let open W in
    let q1 = Uri.of_string (base_url ("notes/" ^ id)) in
    call_endpoint q1 >>| fun res ->
    res |> JSON.from_string |> note_of_json true
  ;;
end

(* Async backend *)
module Async_web
  : Web_deferred with type 'a t = 'a Async.Std.Deferred.t
= struct
  module HTTP = Cohttp_async

  type 'a t = 'a Async.Std.Deferred.t

  let (>>=) = Async.Std.(>>=)

  let (>>|) = Async.Std.(>>|)

  let get (uri : Uri.t) =
    HTTP.Client.get uri >>= fun (_, body) -> body |> HTTP.Body.to_string
  ;;
end

module AsyncAPI
  : (API with type 'a deferred := 'a Async.Std.Deferred.t)
= Make_API(Async_web)

(* Lwt backend *)
module Lwt_web
  : Web_deferred with type 'a t = 'a Lwt.t
= struct
  module HTTP = Cohttp_lwt_unix
  module Body = Cohttp_lwt_body

  type 'a t = 'a Lwt.t

  let (>>=) = Lwt.(>>=)

  let (>>|) = Lwt.(>|=)

  let get (uri : Uri.t) =
    HTTP.Client.get uri >>= fun (_, body) -> body |> Body.to_string
  ;;
end

module LwtAPI
  : (API with type 'a deferred := 'a Lwt.t)
= Make_API(Lwt_web)

