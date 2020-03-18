(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2020 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                                                                          *)
(*  This program is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU General Public License as published by    *)
(*  the Free Software Foundation, either version 3 of the License, or       *)
(*  (at your option) any later version.                                     *)
(*                                                                          *)
(*  This program is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*  GNU General Public License for more details.                            *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(****************************************************************************)

open Js_of_ocaml
open Liquidity_js_michelson

let failwith fmt = Format.kasprintf failwith fmt

let js_failwith fmt =
  Format.kasprintf
    (fun s ->
       Js.raise_js_error (jsnew Js.error_constr (Js.string s)))
    fmt

(* Ensure required libraries are available *)

let is_node_js =
  Js.Optdef.test Js.Unsafe.global##module_ &&
  Js.Optdef.test Js.Unsafe.global##module_##exports

let () =
  if not (Js.Optdef.test Js.Unsafe.global##_XMLHttpRequest ||
          Js.Optdef.test Js.Unsafe.global##activeXObject)
  then
    if is_node_js then
      js_failwith
        "Library xmlhttprequest is required but not available, \
         load it before liquidity-js with:\n\
         const XMLHttpRequest = require('xmlhttprequest').XMLHttpRequest;"
    else
      js_failwith
        "Unexpected: XMLHttpRequest or activeXObject should \
         be available in browser"

(* Configure *)
let xhr_post host ~data path =
  let url = host () ^ path in
  Lwt.catch (fun () ->
      Lwt_request.post
        ~headers:["Content-Type", "application/json"]
        ~get_args:[]
        ~url ~body:data
    )
    (function
      | Lwt_request.Request_failed (code, str) ->
          Lwt.fail (LiquidClientRequest.RequestError (code, str))
      | exn -> Lwt.fail exn
    )

let xhr_get host path =
  let url = host () ^ path in
  Lwt.catch (fun () ->
      Lwt_request.get
        ~headers:["Content-Type", "application/json"]
        ~args:[]
        ~url
    )
    (function
      | Lwt_request.Request_failed (code, str) ->
          Lwt.fail (LiquidClientRequest.RequestError (code, str))
      | exn -> Lwt.fail exn
    )

(* Set RPC call functions to Javascript ones *)
let () =
  LiquidClientRequest.post := xhr_post (fun () -> !LiquidOptions.node);
  LiquidClientRequest.get := xhr_get (fun () -> !LiquidOptions.node)

let mk_option (from_js : 'b -> 'a) (to_js : 'a -> 'b) (r : 'a ref) = jsobject
method get = (to_js !r : 'b)
method set (v : 'b) =
  try r := from_js v; Js.undefined
  with e -> js_failwith "%s" (Printexc.to_string e)
end

let mk_opt_option from_js to_js (r : 'a option ref) =
  mk_option
    (fun x -> match Js.Opt.to_option x with
       | None -> None
       | Some x -> Some (from_js x))
    (function
      | None -> Js.Opt.option None
      | Some x -> Js.Opt.option (Some (to_js x))) r

let mk_bool_option r = mk_option Js.to_bool Js.bool r
let mk_not_bool_option r =
  mk_option (fun v -> not (Js.to_bool v)) (fun v -> Js.bool (not v)) r
let mk_string_option r = mk_option Js.to_string Js.string r
let mk_int_option (r : int ref) = mk_option (fun x -> x) (fun x -> x) r
let mk_string_opt_option r = mk_opt_option Js.to_string Js.string r
let mk_tez_option r =
  mk_option
    (fun x -> LiquidNumber.tez_of_liq (Js.to_string x##toString()))
    (fun x -> Js.string (LiquidNumber.liq_of_tez x)) r
let mk_tez_opt_option r =
  mk_opt_option
    (fun x -> LiquidNumber.tez_of_liq (Js.to_string x##toString()))
    (fun x -> Js.string (LiquidNumber.liq_of_tez x)) r
let mk_int_opt_option (r (* : int option ref *)) = mk_opt_option (fun x -> x) (fun x -> x) r
let mk_network_option r =
  mk_option
    (fun x -> match String.lowercase_ascii @@ Js.to_string x with
       | "dune" | "dune network" | "dune_network" -> LiquidOptions.Dune_network
       | "tezos" -> LiquidOptions.Tezos_network
       | s -> js_failwith "Bad value for network: %s" s)
    (function
      | LiquidOptions.Dune_network -> Js.string "dune"
      | LiquidOptions.Tezos_network -> Js.string "tezos") r


let () =
  let open LiquidOptions in
  Js.export "options" @@ jsobject
val inline = mk_bool_option inline
val simplify = mk_bool_option simplify
val peephole = mk_bool_option peephole
val typeonly = mk_bool_option typeonly
val parseonly = mk_bool_option parseonly
val singleline = mk_bool_option singleline
val ignore_annots_ = mk_bool_option ignore_annots
val retry_without_annots_ = mk_bool_option retry_without_annots
val no_annot_ = mk_bool_option no_annot
val no_uncurrying_ = mk_bool_option no_uncurrying
val main = mk_string_opt_option main
val reason_syntax_ = mk_not_bool_option ocaml_syntax
val writeinfo = mk_bool_option writeinfo
val signature = mk_string_opt_option signature
val node = mk_string_option node
val source = mk_string_opt_option source
val private_key_ = mk_string_opt_option private_key
val public_key_ = mk_string_opt_option public_key
val amount = mk_tez_option amount
val fee = mk_tez_opt_option fee
val gas_limit_ = mk_int_opt_option gas_limit
val storage_limit_ = mk_int_opt_option storage_limit
val counter = mk_int_opt_option counter
val network = mk_network_option network
end

let () =
  Js.export "compiler" @@ jsobject
method compile js_str =
  Js.to_string js_str
  |> compile_to_string
  |> Js.string
method decompile js_str =
  Js.to_string js_str
  |> decompile_string
  |> Js.string
end

let () =
  Js.export "obj_compiler" @@ jsobject
method compile js_str =
  Js.to_string js_str
  |> compile_to_json
  |> Ezjsonm.value_to_js
method decompile obj =
  Ezjsonm.value_from_js obj
  |> decompile_json
  |> Js.string
end

module Client = LiquidityToMichelsonClient.String.Async
module ClientJson = LiquidityToMichelsonClient.SJson.Async
open LiquidityToMichelson
open Lwt.Infix

(* Helper functions *)
open Js_of_ocaml

let new_promise f =
  Js.Unsafe.new_obj Js.Unsafe.global##_Promise [|Js.Unsafe.inject f|]

let mk_js_error fmt =
  Format.kasprintf
    (fun str -> jsnew Js.error_constr (Js.string str))
    fmt

let report_err ?(kind="Liquidity Error") (err_loc, err_msg) =
  mk_js_error
    "%a: %s: @[%s@]"
    LiquidLoc.print_loc err_loc kind err_msg

let report_error = function
  | LiquidTypes.LiquidError error -> report_err (error.err_loc, error.err_msg);
  | LiquidNamespace.Unknown_namespace (p, err_loc) ->
    report_err
      (err_loc,
       Printf.sprintf "Unknown module or contract %s" (String.concat "." p))
  | LiquidFromMicheline.Missing_program_field f ->
    mk_js_error "Missing script field %s" f
  | Failure f -> mk_js_error "Failure: %s" f
  | Syntaxerr.Error (Syntaxerr.Other loc) ->
    report_err ~kind:"Liquidity Syntax Error"
      (LiquidLoc.loc_of_location loc, "unknown");
  | exn ->
    let backtrace = Printexc.get_backtrace () in
    mk_js_error "Liquidity Error: %s\nBacktrace:\n%s"
      (Printexc.to_string exn) backtrace

let lwt_to_promise f =
  new_promise @@ fun resolve reject ->
  let reject_exn exn = Lwt.return @@ reject (report_error exn) in
  Lwt.async @@ fun () ->
  Lwt.catch
    (fun () ->
       try f () >|= resolve
       with exn -> reject_exn exn)
    reject_exn

let js_to_contract j =
  try
    match Js.to_string (Js.typeof j) with
    | "object" ->
      From_strings
        (Array.to_list
           (Js.to_array (Js.Unsafe.coerce j : 'a Js.js_array Js.t))
         |> List.map Js.to_string)
    | "string" ->
      From_strings [
        Js.to_string (Js.Unsafe.coerce j : Js.js_string Js.t)
      ]
    | _ -> raise Not_found
  with _ ->
    failwith "Bad contract argument: %s" j##toString()

let to_string_list j =
  try
    Array.to_list
      (Js.to_array (Js.Unsafe.coerce j : 'a Js.js_array Js.t))
    |> List.map Js.to_string
  with _ ->
    failwith "Must be an array of strings: %s" j##toString()

let to_int j = Js.parseInt j##toString()

let to_datatype j =
  Js.to_string j
  |> LiquidityToMichelsonClient.String.C.SourceConv.parse_datatype

let uint8array_of_bytes b =
  let l = Bytes.length b in
  let u = jsnew Typed_array.uint8Array (l) in
  Bytes.iteri (fun i c ->
      Typed_array.set u i (Char.code c)
    ) b;
  u

let arg ?default name translate j =
  match default, Js.to_string (Js.typeof j) with
  | None, "undefined" -> failwith "Missing argument %s" name
  | Some default, "undefined" -> default
  | _ -> translate j

let field ?default name translate j =
  match default, Js.to_string (Js.typeof j) with
  | None, "undefined" -> failwith "Missing object field %s" name
  | Some default, "undefined" -> default
  | _ -> translate j

(* Client export *)

let () =
  Js.export "client" @@ jsobject

method init_storage_ o =
  lwt_to_promise @@ fun () ->
  Client.init_storage
    (field "code" js_to_contract o##code)
    (field "arguments" ~default:[] to_string_list o##arguments)
  >|= Js.string


method init_storage_obj_ o =
  lwt_to_promise @@ fun () ->
  ClientJson.init_storage
    (field "code" js_to_contract o##code)
    (field "arguments" ~default:[] to_string_list o##arguments)
  >|= Ezjsonm.value_to_js


method run o =
  lwt_to_promise @@ fun () ->
  Client.run
    (field "code" js_to_contract o##code)
    (field "entrypoint" ~default:"default" Js.to_string o##entrypoint)
    (field "parameter" Js.to_string o##parameter)
    (field "storage" Js.to_string o##storage)
  >|= fun (ops, storage, bm_diff) ->
  (* TODO: ops, bm *)
  Js.string storage


method trace o =
  lwt_to_promise @@ fun () ->
  Client.run_debug
    (field "code" js_to_contract o##code)
    (field "entrypoint" ~default:"default" Js.to_string o##entrypoint)
    (field "parameter" Js.to_string o##parameter)
    (field "storage" Js.to_string o##storage)
  >|= fun (ops, storage, bm_diff, trace) ->
  (* TODO: ops, bm, trace *)
  Js.string storage

method deploy o =
  lwt_to_promise @@ fun () ->
  Client.deploy
    (field "code" js_to_contract o##code)
    (field "arguments" ~default:[] to_string_list o##arguments)
  >|= fun (oph, c) ->
  jsobject
    val operation_hash_ = Js.string oph
    val contract = Js.string c
  end

method get_storage_ o =
  lwt_to_promise @@ fun () ->
  Client.get_storage
    (field "code" js_to_contract o##code)
    (field "address" Js.to_string o##address)
  >|= Js.string

method get_big_map_value_ o =
  lwt_to_promise @@ fun () ->
  Client.get_big_map_value
    ((field "big_map_id" (fun x -> LiquidClientSigs.Bm_id (to_int x)) o##big_map_id_),
     (field "key_type" to_datatype o##key_type_),
     (field "value_type" to_datatype o##value_type_))
    (field "key" Js.to_string o##key)
  >|= function
  | None -> Js.Opt.empty
  | Some v -> Js.Opt.return (Js.string v)

method call o =
  lwt_to_promise @@ fun () ->
  Client.call
    ?contract:(field "code" ~default:None
                 (fun c -> Some (js_to_contract c))
                 o##code)
    ~address:(field "address" Js.to_string o##address)
    ~entry:(field "entrypoint" ~default:"default" Js.to_string o##entry)
    (field "parameter" Js.to_string o##parameter)
  >|= Js.string

method pack o =
  lwt_to_promise @@ fun () ->
  Client.pack
    ~const:(field "value" Js.to_string o##value)
    ~ty:(field "type" Js.to_string o##type_)
  >|= fun b -> Hex.(show @@ of_bytes b) |> Js.string

method forge_deploy_ o =
  lwt_to_promise @@ fun () ->
  Client.forge_deploy
    (field "code" js_to_contract o##code)
    (field "arguments" ~default:[] to_string_list o##arguments)
  >|= fun b -> Hex.(show @@ of_bytes b) |> Js.string

method forge_call_ o =
  lwt_to_promise @@ fun () ->
  Client.forge_call
    ?contract:(field "code" ~default:None
                 (fun c -> Some (js_to_contract c))
                 o##code)
    ~address:(field "address" Js.to_string o##address)
    ~entry:(field "entrypoint" ~default:"default" Js.to_string o##entrypoint)
    (field "parameter" Js.to_string o##parameter)
  >|= fun b -> Hex.(show @@ of_bytes b) |> Js.string

end


let () =
  Js.export "ready" @@ new_promise @@ fun resolve _ ->
  let sodium = Js.Unsafe.global##sodium in
  sodium##ready##then_(fun _ ->
      resolve Js.Optdef.empty
    )
