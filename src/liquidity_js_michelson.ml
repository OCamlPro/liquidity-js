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

open LiquidTypes
open LiquidityToMichelson
module Client = LiquidityToMichelsonClient.Multi

let compile_aux (liq : string) =
  let multi_liq = Lazy_superposed.string liq in
  let liq_ast = LiquidityContractMulti.force_ast multi_liq in
  let mic_ast, comp_init, _ = Client.L.compile_contract liq_ast in
  Lazy_superposed.ast mic_ast

let decompile_aux multi_mic =
  let mic_ast = MichelsonContractMulti.force_ast multi_mic in
  let liq_ast = Client.L.decompile_contract mic_ast in
  let multi_liq = Lazy_superposed.ast liq_ast in
  let liq_string = LiquidityContractMulti.force_string multi_liq in
  let liq_string =
    if !LiquidOptions.writeinfo then
      LiquidInfomark.gen_info ~decompile:true [] ^ liq_string
    else liq_string in
  liq_string (* , liq_ast *)

let decompile_aux source =
  try
    LiquidOptions.ignore_annots := false;
    decompile_aux source
  with exn ->
    (* Rety and ignore annotations if failing *)
    if !LiquidOptions.ignore_annots then raise exn;
    Format.eprintf
      "Decompilation failed, retrying and ignoring \
       Michelson type annotations@.";
    LiquidOptions.ignore_annots := true;
    let mic = decompile_aux source in
    LiquidOptions.ignore_annots := false;
    mic

let compile_to_string (liq: string) =
  let multi_mic = compile_aux liq in
  let mic_string = MichelsonContractMulti.force_string multi_mic in
  let mic_string =
    if !LiquidOptions.writeinfo then
      LiquidInfomark.gen_info ~decompile:false [] ^ mic_string
    else mic_string in
  mic_string

let decompile_string (mic : string) =
  let multi_mic = Lazy_superposed.string mic in
  decompile_aux multi_mic

let compile_to_json (liq: string) =
  let multi_mic = compile_aux liq in
  MichelsonContractMulti.force_json multi_mic

let decompile_json (mic : Ezjsonm.value) =
  let multi_mic = Lazy_superposed.json mic in
  decompile_aux multi_mic
