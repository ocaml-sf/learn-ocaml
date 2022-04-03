open Ppxlib

let pattern_samplers =
  object
    inherit [string list] Ast_traverse.fold as super

    method! pattern p acc =
      let acc = super#pattern p acc in
      match p.ppat_desc with
      | Ppat_var var -> (
          match String.index_opt var.txt '_' with
          | Some i when String.sub var.txt 0 i = "sample" ->
              let suffix =
                String.sub var.txt (i + 1) (String.length var.txt - i - 1)
              in
              suffix :: acc
          | _ -> acc)
      | _ -> acc
  end

let rec get_samplers bindings acc =
  match bindings with
  | [] -> List.rev @@ List.flatten acc
  | binding :: rest ->
      get_samplers rest @@ (pattern_samplers#pattern binding.pvb_pat [] :: acc)

module Ast_builder = Ast_builder.Make (struct
  let loc = Location.none
end)

let sampler_recorder s =
  let open Ast_builder in
  let create_samplers_registration samplers =
    let sampler_expr sampler =
      pexp_apply
        (evar @@ "Introspection.register_sampler") 
        [ Nolabel,estring sampler
        ; Nolabel,evar @@ "sample_" ^ sampler]
    in
    let samplers_registration = List.map sampler_expr samplers |> esequence in
    let register_toplevel =
      [ value_binding ~pat:punit ~expr:samplers_registration ]
    in
    pstr_value Nonrecursive register_toplevel
  in
  List.fold_right
    (fun si acc ->
      match si.pstr_desc with
      | Pstr_value (_, bindings) -> (
          match get_samplers bindings [] with
          | [] -> si :: acc
          | samplers -> si :: create_samplers_registration samplers :: acc)
      | _ -> si :: acc)
    s []

let expand = sampler_recorder
