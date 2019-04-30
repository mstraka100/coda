open Core

let least_out_of_scope_field_element = ref None

let null =
  let open Snark_params.Tick.Field in
  let open Snark_params.Tick.Inner_curve.Coefficients in
  let rec compute_least_out_of_scope_field_element x =
    if is_square ((x * x * x) + (a * x) + b) then
      compute_least_out_of_scope_field_element (x + one)
    else x
  in
  if Option.is_some !least_out_of_scope_field_element then
    Option.value_exn !least_out_of_scope_field_element
  else (
    least_out_of_scope_field_element :=
      Some (compute_least_out_of_scope_field_element zero) ;
    Option.value_exn !least_out_of_scope_field_element )
