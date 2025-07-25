(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2024 OCaml Software Foundation.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(* LTI OAuth signature verification: expect-style tests *)

open Learnocaml_auth.LtiAuth
open Learnocaml_store
open Lwt.Infix

let () =
  sync_dir := Filename.get_temp_dir_name ();
  LtiIndex.repo_path := "mem://"

let parse_body (body : string) : (string * string) list =
  Uri.query_of_encoded body
  |> List.map (fun (k, vs) -> k, String.concat "," vs)

let learner_payload =
  "oauth_version=1.0&oauth_nonce=435e97b4b067d2c6b629d8300a2400a2&oauth_timestamp=1753433334&oauth_consumer_key=moodle.univ-tlse3.fr&user_id=2&lis_person_sourcedid=&roles=Learner&context_id=2&context_label=Pfi&context_title=Pfitaxel&resource_link_title=Pfi&resource_link_description=&resource_link_id=1&context_type=CourseSection&lis_course_section_sourcedid=&lis_result_sourcedid=%7B%22data%22%3A%7B%22instanceid%22%3A%221%22%2C%22userid%22%3A%222%22%2C%22typeid%22%3Anull%2C%22launchid%22%3A1397134956%7D%2C%22hash%22%3A%2213aeb6940f7f79b55c9ff49c1690352ea478abd188cd1a44ec51a1e916034dd4%22%7D&lis_outcome_service_url=http%3A%2F%2Flocalhost%3A9090%2Fmod%2Flti%2Fservice.php&lis_person_name_given=Admin&lis_person_name_family=User&lis_person_name_full=Admin+User&ext_user_username=user&lis_person_contact_email_primary=user%40example.com&launch_presentation_locale=en&ext_lms=moodle-2&tool_consumer_info_product_family_code=moodle&tool_consumer_info_version=2021051707&oauth_callback=about%3Ablank&lti_version=LTI-1p0&lti_message_type=basic-lti-launch-request&tool_consumer_instance_guid=localhost&tool_consumer_instance_name=New+Site&tool_consumer_instance_description=New+Site&launch_presentation_document_target=iframe&launch_presentation_return_url=http%3A%2F%2Flocalhost%3A9090%2Fmod%2Flti%2Freturn.php%3Fcourse%3D2%26launch_container%3D2%26instanceid%3D1%26sesskey%3DzWWyXZqOnc&oauth_signature_method=HMAC-SHA1&oauth_signature=vtyWSL1j2qBCGdzti7AP%2BaqH2WU%3D"
let instructor_payload =
  "oauth_version=1.0&oauth_nonce=8f0c975b428f3a6683947a5348aaabad&oauth_timestamp=1753432816&oauth_consumer_key=moodle.univ-tlse3.fr&user_id=2&lis_person_sourcedid=&roles=Instructor%2Curn%3Alti%3Asysrole%3Aims%2Flis%2FAdministrator%2Curn%3Alti%3Ainstrole%3Aims%2Flis%2FAdministrator&context_id=2&context_label=Pfi&context_title=Pfitaxel&resource_link_title=Pfi&resource_link_description=&resource_link_id=1&context_type=CourseSection&lis_course_section_sourcedid=&lis_result_sourcedid=%7B%22data%22%3A%7B%22instanceid%22%3A%221%22%2C%22userid%22%3A%222%22%2C%22typeid%22%3Anull%2C%22launchid%22%3A739795865%7D%2C%22hash%22%3A%226917e44d8a3e77beec8cd7f1e3df1e0c174cdb1a7b61af2728a309f7cf5b661c%22%7D&lis_outcome_service_url=http%3A%2F%2Flocalhost%3A9090%2Fmod%2Flti%2Fservice.php&lis_person_name_given=Admin&lis_person_name_family=User&lis_person_name_full=Admin+User&ext_user_username=user&lis_person_contact_email_primary=user%40example.com&launch_presentation_locale=en&ext_lms=moodle-2&tool_consumer_info_product_family_code=moodle&tool_consumer_info_version=2021051707&oauth_callback=about%3Ablank&lti_version=LTI-1p0&lti_message_type=basic-lti-launch-request&tool_consumer_instance_guid=localhost&tool_consumer_instance_name=New+Site&tool_consumer_instance_description=New+Site&launch_presentation_document_target=iframe&launch_presentation_return_url=http%3A%2F%2Flocalhost%3A9090%2Fmod%2Flti%2Freturn.php%3Fcourse%3D2%26launch_container%3D2%26instanceid%3D1%26sesskey%3DzWWyXZqOnc&oauth_signature_method=HMAC-SHA1&oauth_signature=ZXYi%2BJOi4CgdkJoao7gdjzP%2FpZE%3D"
let false_payload =
  "oauth_version=1.0&oauth_nonce=8f0c975b428f3a7683947a5348aaabad&oauth_timestamp=1&oauth_consumer_key=moodle.univ-tlse3.fr&user_id=2&lis_person_sourcedid=&roles=Instructor%2Curn%3Alti%3Asysrole%3Aims%2Flis%2FAdministrator%2Curn%3Alti%3Ainstrole%3Aims%2Flis%2FAdministrator&context_id=2&context_label=Pfi&context_title=Pfitaxel&resource_link_title=Pfi&resource_link_description=&resource_link_id=1&context_type=CourseSection&lis_course_section_sourcedid=&lis_result_sourcedid=%7B%22data%22%3A%7B%22instanceid%22%3A%221%22%2C%22userid%22%3A%222%22%2C%22typeid%22%3Anull%2C%22launchid%22%3A739795865%7D%2C%22hash%22%3A%226917e44d8a3e77beec8cd7f1e3df1e0c174cdb1a7b61af2728a309f7cf5b661c%22%7D&lis_outcome_service_url=http%3A%2F%2Flocalhost%3A9090%2Fmod%2Flti%2Fservice.php&lis_person_name_given=Admin&lis_person_name_family=User&lis_person_name_full=Admin+User&ext_user_username=user&lis_person_contact_email_primary=user%40example.com&launch_presentation_locale=en&ext_lms=moodle-2&tool_consumer_info_product_family_code=moodle&tool_consumer_info_version=2021051707&oauth_callback=about%3Ablank&lti_version=LTI-1p0&lti_message_type=basic-lti-launch-request&tool_consumer_instance_guid=localhost&tool_consumer_instance_name=New+Site&tool_consumer_instance_description=New+Site&launch_presentation_document_target=iframe&launch_presentation_return_url=http%3A%2F%2Flocalhost%3A9090%2Fmod%2Flti%2Freturn.php%3Fcourse%3D2%26launch_container%3D2%26instanceid%3D1%26sesskey%3DzWWyXZqOnc&oauth_signature_method=HMAC-SHA1&oauth_signature=ZXYi%2BJOi4CgdkJoao7gdjzP%2FpZE%3D"

let launch_url = "http://localhost:8080/launch"
let consumer_key = "moodle.univ-tlse3.fr"
let shared_secret = "5e06d2c671b7aaf26678bb52dd085f128cda772357ab11c5f5f12b87b0ef6f0b"

(* Unit test: verifies that the OAuth signature is valid for an Instructor *)
let%expect_test "LTI: instructor signature is valid" =
  let params = parse_body instructor_payload in
  Lwt_main.run (
    check_oauth launch_url params shared_secret >>= function
    | Ok id ->
      Printf.printf "LTI accepted: %s\n" id;
      Lwt.return_unit
    | Error msg ->
      Printf.printf "LTI rejected: %s\n" msg;
      Lwt.return_unit
  );
  [%expect {| LTI accepted: moodle.univ-tlse3.fr/2 |}]

(* Unit test: verifies that the OAuth signature is valid for a Learner *)
let%expect_test "LTI: learner signature is valid" =
  let params = parse_body learner_payload in
  Lwt_main.run (
    check_oauth launch_url params shared_secret >>= function
    | Ok id ->
      Printf.printf "LTI accepted: %s\n" id;
      Lwt.return_unit
    | Error msg ->
      Printf.printf "LTI rejected: %s\n" msg;
      Lwt.return_unit
  );
  [%expect {| LTI accepted: moodle.univ-tlse3.fr/2 |}]

(* Unit test: verifies that an invalid OAuth signature is correctly rejected *)
let%expect_test "LTI: invalid signature is rejected" =
  let params = parse_body false_payload in
  Lwt_main.run (
    check_oauth launch_url params shared_secret >>= function
    | Ok id ->
      Printf.printf "LTI accepted (should have failed!): %s\n" id;
      Lwt.return_unit
    | Error msg ->
      Printf.printf "LTI rejected: %s\n" msg;
      Lwt.return_unit
  );
  [%expect {|
    LTI rejected: Wrong signature
  |}]
