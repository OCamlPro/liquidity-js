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
val enumerable = Js._true (* Show getter/setter in object *)
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

let public_key_option =
  let open Dune_Network_Lib in
  mk_opt_option
    (fun j ->
       let s = Js.to_string j in
       match Ed25519.Public_key.of_b58check_opt s with
       | None -> js_failwith "Bad public key (must be edpk)."
       | Some k ->
           let pkh = match !LiquidOptions.network with
             | Dune_network ->
               Ed25519.Public_key_hash_dune.of_public_key k
               |> Ed25519.Public_key_hash_dune.to_b58check
             | Tezos_network ->
               Ed25519.Public_key_hash_tezos.of_public_key k
               |> Ed25519.Public_key_hash_tezos.to_b58check in
           LiquidOptions.source := Some pkh;
           s
    )
    Js.string
    LiquidOptions.public_key

let private_key_option =
  let open Dune_Network_Lib in
  mk_opt_option
    (fun j ->
       let s = Js.to_string j in
       match Dune_Network_Lib.Ed25519.Secret_key.of_b58check_opt s with
       | None -> js_failwith "Bad private key (must be edsk)."
       | Some sk ->
           let k = Sodium.Sign.secret_key_to_public_key sk
                   |> Ed25519.Public_key.to_b58check in
           public_key_option##set(Js.Opt.return (Js.string k)) |> ignore;
           s
    )
    Js.string
    LiquidOptions.private_key

let options_obj = jsobject end

let _Object = Js.Unsafe.global##_Object

let () =
  let open LiquidOptions in
  let add_option prop_name property =
    _Object##defineProperty(options_obj, Js.string prop_name, property)
    |> ignore
  in
  add_option "inline" (mk_bool_option inline);
  add_option "simplify" (mk_bool_option simplify);
  add_option "peephole" (mk_bool_option peephole);
  add_option "typeonly" (mk_bool_option typeonly);
  add_option "parseonly" (mk_bool_option parseonly);
  add_option "singleline" (mk_bool_option singleline);
  add_option "ignore_annots" (mk_bool_option ignore_annots);
  add_option "retry_without_annots" (mk_bool_option retry_without_annots);
  add_option "no_annot" (mk_bool_option no_annot);
  add_option "no_uncurrying" (mk_bool_option no_uncurrying);
  add_option "main" (mk_string_opt_option main);
  add_option "reason_syntax" (mk_not_bool_option ocaml_syntax);
  add_option "writeinfo" (mk_bool_option writeinfo);
  add_option "signature" (mk_string_opt_option signature);
  add_option "node" (mk_string_option node);
  add_option "source" (mk_string_opt_option source);
  add_option "private_key" (private_key_option);
  add_option "public_key" (public_key_option);
  add_option "network" (mk_network_option network);
  ()

let () =
  Js.export "options" options_obj

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

let () =
  Js.export "compiler" @@ jsobject
method compile js_str =
  try
    Js.to_string js_str
    |> compile_to_string
    |> Js.string
  with exn -> Js.raise_js_error (report_error exn)
method decompile js_str =
  try
    Js.to_string js_str
    |> decompile_string
    |> Js.string
  with exn -> Js.raise_js_error (report_error exn)
end

let () =
  Js.export "obj_compiler" @@ jsobject
method compile js_str =
  try
    Js.to_string js_str
    |> compile_to_json
    |> Ezjsonm.value_to_js
  with exn -> Js.raise_js_error (report_error exn)
method decompile obj =
  try
    Ezjsonm.value_from_js obj
    |> decompile_json
    |> Js.string
  with exn -> Js.raise_js_error (report_error exn)
end

module Client = LiquidityToMichelsonClient.String.Async
module ClientJson = LiquidityToMichelsonClient.SJson.Async
open LiquidityToMichelson
open Lwt.Infix

(* Helper functions *)
open Js_of_ocaml

let new_promise f =
  Js.Unsafe.new_obj Js.Unsafe.global##_Promise [|Js.Unsafe.inject f|]

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

let js_to_tez j = LiquidNumber.tez_of_liq (Js.to_string j##toString())

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

(* Datastructures conversion to JS *)
open LiquidityToMichelsonClient.String.T

let internal_operation_to_js op =
  let js_op =
    Json_encoding.construct SourceOperation.internal_encoding op
    |> Ezjsonm.value_to_js
    |> Js.Unsafe.coerce in
  js_op##amount <-
    js_op##amount
    |> Js.to_string
    |> Z.of_string
    |> LiquidNumber.tez_of_mic_mutez
    |> LiquidNumber.liq_of_tez
    |> (fun s -> s ^ LiquidOptions.curreny ())
    |> Js.string;
  js_op

let big_map_diff_to_js bmdiff =
  let open Json_encoding in
  construct
    (Big_map_diff.encoding
       (union [
           case (obj1 (req "id" int))
             (function
               | LiquidClientSigs.Bm_id i -> Some i
               | _ -> None)
             (fun i -> LiquidClientSigs.Bm_id i);
           case (obj2
                   (req "id" int)
                   (req "name" string))
             (function
               | LiquidClientSigs.Bm_name (i, n) -> Some (i, n)
               | _ -> None)
             (fun (i, n) -> LiquidClientSigs.Bm_name (i, n));

         ])
       string)
    bmdiff
  |> Ezjsonm.value_to_js

let trace_to_js tr =
  let open Json_encoding in
  let open LiquidTypes in
  construct
    (Trace.encoding
       (conv
          (function
            | { loc_file = "<unspecified>"; loc_pos  = None } -> None
            | { loc_file ; loc_pos } -> Some (loc_file, loc_pos))
         (function
           | None -> { loc_file = "<unspecified>"; loc_pos  = None }
           | Some (loc_file, loc_pos) -> { loc_file; loc_pos })
         (option
            (obj2
               (req "file" string)
               (opt "loc" (tup2 (tup2 int int) (tup2 int int))))))
       string)
    tr
  |> Ezjsonm.value_to_js


let set_op_options o =
  let fee =
    field "fee" ~default:None (fun x -> Some (js_to_tez x)) o##fee in
  let gas_limit =
    field "gas_limit" ~default:None (fun x -> Some (to_int x)) o##gas_limit_ in
  let storage_limit =
    field "storage_limit" ~default:None (fun x -> Some (to_int x)) o##storage_limit_ in
  let counter =
    field "counter" ~default:None (fun x -> Some (to_int x)) o##counter in
  LiquidOptions.fee := fee;
  LiquidOptions.gas_limit := gas_limit;
  LiquidOptions.storage_limit := storage_limit;
  LiquidOptions.counter := counter;
  ()

let allowed_fields o l =
  Array.iter (fun prop ->
      let prop = Js.to_string prop in
      if not @@ List.mem prop l then
        failwith "Unsupported object field %s (allowed fields: %s)"
          prop (String.concat ", " (List.sort Pervasives.compare l))
    ) (Js.to_array _Object##keys(o))

(* Client export *)

let () =
  Js.export "client" @@ jsobject

method init_storage_ o =
  lwt_to_promise @@ fun () ->
  allowed_fields o ["code"; "arguments"];
  Client.init_storage
    (field "code" js_to_contract o##code)
    (field "arguments" ~default:[] to_string_list o##arguments)
  >|= Js.string


method init_storage_obj_ o =
  lwt_to_promise @@ fun () ->
  allowed_fields o ["code"; "arguments"];
  ClientJson.init_storage
    (field "code" js_to_contract o##code)
    (field "arguments" ~default:[] to_string_list o##arguments)
  >|= Ezjsonm.value_to_js


method run o =
  lwt_to_promise @@ fun () ->
  allowed_fields o ["amount"; "code"; "entrypoint"; "parameter"; "storage"];
  Client.run
    ~amount:(field "amount" js_to_tez o##amount)
    (field "code" js_to_contract o##code)
    (field "entrypoint" ~default:"default" Js.to_string o##entrypoint)
    (field "parameter" Js.to_string o##parameter)
    (field "storage" Js.to_string o##storage)
  >|= fun (ops, storage, bm_diff) ->
  jsobject
    val storage = Js.string storage
    val operations =
      Js.Optdef.return @@ Js.array @@ Array.of_list @@
      List.map internal_operation_to_js ops
    val big_map_diff_ = big_map_diff_to_js bm_diff
  end


method trace o =
  lwt_to_promise @@ fun () ->
  allowed_fields o ["amount"; "code"; "entrypoint"; "parameter"; "storage"];
  Client.run_debug
    ~amount:(field "amount" js_to_tez o##amount)
    (field "code" js_to_contract o##code)
    (field "entrypoint" ~default:"default" Js.to_string o##entrypoint)
    (field "parameter" Js.to_string o##parameter)
    (field "storage" Js.to_string o##storage)
  >|= fun (ops, storage, bm_diff, trace) ->
  jsobject
    val storage = Js.string storage
    val operations =
      Js.Optdef.return @@ Js.array @@ Array.of_list @@
      List.map internal_operation_to_js ops
    val big_map_diff_ = big_map_diff_to_js bm_diff
    val trace = trace_to_js trace
  end

method deploy o =
  lwt_to_promise @@ fun () ->
  allowed_fields o ["balance"; "code"; "arguments";
                    "counter"; "fee"; "gas_limit"; "storage_limit"];
  set_op_options o;
  Client.deploy
    ~balance:(field "balance" js_to_tez o##balance)
    (field "code" js_to_contract o##code)
    (field "arguments" ~default:[] to_string_list o##arguments)
  >|= fun (oph, c) ->
  jsobject
    val operation_hash_ = Js.string oph
    val contract = Js.string c
  end

method get_storage_ o =
  lwt_to_promise @@ fun () ->
  allowed_fields o ["code"; "address"];
  Client.get_storage
    (field "code" js_to_contract o##code)
    (field "address" Js.to_string o##address)
  >|= Js.string

method get_big_map_value_ o =
  lwt_to_promise @@ fun () ->
  allowed_fields o ["big_map_id"; "key_type"; "value_type"; "key"];
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
  allowed_fields o ["code"; "amount"; "address"; "entrypoint"; "parameter";
                    "counter"; "fee"; "gas_limit"; "storage_limit"];
  set_op_options o;
  Client.call
    ?contract:(field "code" ~default:None
                 (fun c -> Some (js_to_contract c))
                 o##code)
    ~amount:(field "amount" js_to_tez o##amount)
    ~address:(field "address" Js.to_string o##address)
    ~entry:(field "entrypoint" ~default:"default" Js.to_string o##entry)
    (field "parameter" Js.to_string o##parameter)
  >|= Js.string

method pack o =
  lwt_to_promise @@ fun () ->
  allowed_fields o ["value"; "type"];
  Client.pack
    ~const:(field "value" Js.to_string o##value)
    ~ty:(field "type" Js.to_string o##type_)
  >|= fun b -> Hex.(show @@ of_bytes b) |> Js.string

method forge_deploy_ o =
  lwt_to_promise @@ fun () ->
  allowed_fields o ["balance"; "code"; "arguments";
                    "counter"; "fee"; "gas_limit"; "storage_limit"];
  set_op_options o;
  Client.forge_deploy
    ~balance:(field "balance" js_to_tez o##balance)
    (field "code" js_to_contract o##code)
    (field "arguments" ~default:[] to_string_list o##arguments)
  >|= fun b -> Hex.(show @@ of_bytes b) |> Js.string

method forge_call_ o =
  lwt_to_promise @@ fun () ->
  allowed_fields o ["code"; "amount"; "address"; "entrypoint"; "parameter";
                    "counter"; "fee"; "gas_limit"; "storage_limit"];
  set_op_options o;
  Client.forge_call
    ?contract:(field "code" ~default:None
                 (fun c -> Some (js_to_contract c))
                 o##code)
    ~amount:(field "amount" js_to_tez o##amount)
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
