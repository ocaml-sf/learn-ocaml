open Learnocaml_common

let pred_bijection s = decode (encode s) = s
let print_encode s = Printf.printf "%s " (encode s)


let testcase_eq = ["=test";"test=test";"test=";"==";""]

let%expect_test "encode '=' as '-e'" = 
  List.iter print_encode testcase_eq;
  [%expect_exact {|-etest test-etest test-e -e-e  |}]

let%test "decode @@ encode == id" = 
  List.for_all pred_bijection testcase_eq


let testcase_co = [",test"; "test,test"; "test,"; ",,"; ""]

let%expect_test "encode ',' as '-c'" = 
  List.iter print_encode testcase_co;
  [%expect_exact {|-ctest test-ctest test-c -c-c  |}]

let%test "decode @@ encode == id" = 
  List.for_all pred_bijection testcase_co


let testcase_an = ["&test"; "test&test"; "test&"; "&&"; ""]

let%expect_test "encode '&' as '-a'" = 
  List.iter print_encode testcase_an;
  [%expect_exact {|-atest test-atest test-a -a-a  |}]

let%test "decode @@ encode == id" = 
  List.for_all pred_bijection testcase_an


let testcase_da = ["-test"; "test-test"; "test-"; "--"; ""]

let%expect_test "encode '-' as '--'" = 
  List.iter print_encode testcase_co;
  [%expect_exact {|--test test--test test-- ----  |}]

let%test "decode @@ encode == id" = 
  List.for_all pred_bijection testcase_da


let testcase_all = ["=,&-";"=,-&";"=&,-";"=&-,";"=-,&";"=-&,";
                    ",=&-";",=-&";",&=-";",&-=";",-=&";",-&=";
                    "&=,-";"&=-,";"&,=-";"&,-=";"&-=,";"&-,=";
                    "-=,&";"-=&,";"-,=&";"-,&=";"-&=,";"-&,="]

let%expect_test "encode all possibilities" = 
  List.iter print_encode testcase_co;
  [%expect_exact {|-e-c-a-- -e-c---a -e-a-c-- -e-a---c -e---c-a -e---a-c -c-e-a-- -c-e---a -c-a-e-- -c-a---e -c---e-a -c---a-e -a-e-c-- -a-e---c -a-c-e-- -a-c---e -a---e-c -a---c-e ---e-c-a ---e-a-c ---c-a-e ---c-a-e ---a-e-c ---a-c-e |}]

let%test "decode @@ encode == id" = 
  List.for_all pred_bijection testcase_all