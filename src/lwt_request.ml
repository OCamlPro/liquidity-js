open Js_of_ocaml_lwt.XmlHttpRequest
open Lwt.Infix

exception Request_failed of (int * string)

let get ?(headers=[]) ~url ~args =
  perform_raw_url ~headers ~get_args:args url
  >>= fun frame ->
  match frame.code with
  | 200 | 204 -> Lwt.return frame.content
  | c -> Lwt.fail (Request_failed (c, frame.content))

let post ?(headers=[]) ?(get_args=[]) ~url ~body =
  perform_raw_url ~headers ~get_args ~contents:(`String body) url
  >>= fun frame ->
  match frame.code with
  | 200 | 204 -> Lwt.return frame.content
  | c -> Lwt.fail (Request_failed (c, frame.content))
