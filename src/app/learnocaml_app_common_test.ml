open Learnocaml_common


let to_test_encode s = Printf.printf "%s " (encode s)
let to_test_decode s = Printf.printf "%s " (decode s)
let to_test_bijection1 s = Printf.printf "%s " (decode (encode s))
let to_test_bijection2 s = Printf.printf "%s " (encode (decode s))

let original_e  =  ["=test";"test=test";"test=";"==";""]
let coded_e = ["-etest";"test-etest";"test-e";"-e-e";""]

let original_c  =  [",test";"test,test";"test,";",,";""]
let coded_c = ["-ctest";"test-ctest";"test-c";"-c-c";""]

let original_a  =  ["&test";"test&test";"test&";"&&";""]
let coded_a = ["-atest";"test-atest";"test-a";"-a-a";""]

let original__  =  ["-test";"test-test";"test-";"--";""]
let coded__ = ["--test";"test--test";"test--";"----";""]

let original_all = ["=,&-";"=,-&";"=&,-";"=&-,";"=-,&";"=-&,";
                    ",=&-";",=-&";",&=-";",&-=";",-=&";",-&=";
                    "&=,-";"&=-,";"&,=-";"&,-=";"&-=,";"&-,=";
                    "-=,&";"-=&,";"-,=&";"-,&=";"-&=,";"-&,="]
let codedall = ["-e-c-a--";"-e-c---a";"-e-a-c--";"-e-a---c";"-e---c-a";"-e---a-c"; 
                "-c-e-a--";"-c-e---a";"-c-a-e--";"-c-a---e";"-c---e-a";"-c---a-e"; 
                "-a-e-c--";"-a-e---c";"-a-c-e--";"-a-c---e";"-a---e-c";"-a---c-e";
                "---e-c-a";"---e-a-c";"---c-a-e";"---c-a-e";"---a-e-c";"---a-c-e"]


let%expect_test "encode -e" = 
  List.iter to_test_encode original_e;
  [%expect {|-etest test-etest test-e -e-e  |}]

let%expect_test "encode -c" = 
  List.iter to_test_encode original_c;
  [%expect {|-ctest test-ctest test-c -c-c  |}]

let%expect_test "encode -a" = 
  List.iter to_test_encode original_a;
  [%expect {|-atest test-atest test-a -a-a  |}]

let%expect_test "encode --" = 
  List.iter to_test_encode original__;
  [%expect {|--test test--test test-- ----  |}]

let%expect_test "encode all" =
  List.iter to_test_encode original_all;
  [%expect {|-e-c-a-- -e-c---a -e-a-c-- -e-a---c -e---c-a -e---a-c -c-e-a-- -c-e---a -c-a-e-- -c-a---e -c---e-a -c---a-e -a-e-c-- -a-e---c -a-c-e-- -a-c---e -a---e-c -a---c-e ---e-c-a ---e-a-c ---c-a-e ---c-a-e ---a-e-c ---a-c-e |}]


let%expect_test "decode -e" = 
  List.iter to_test_decode coded_e;
  [%expect {|=test test=test test= ==  |}]

let%expect_test "decode -c" = 
  List.iter to_test_decode coded_c;
  [%expect {|,test test,test test, ,,  |}]

let%expect_test "decode -a" = 
  List.iter to_test_decode coded_a;
  [%expect {|&test test&test test& &&  |}]

let%expect_test "decode --" = 
  List.iter to_test_decode coded_e;
  [%expect {|-test test-test test- --  |}]

let%expect_test "decode all" =
  List.iter to_test_decode codedall;
  [%expect {|=,&- =,-& =&,- =&-, =-,& =-&, ,=&- ,=-& ,&=- ,&-= ,-=& ,-&= &=,- &=-, &,=- &,-= &-=, &-,= -=,& -=&, -,=& -,&= -&=, -&,= |}]


let%expect_test "bijection 1 -e" = 
  List.iter to_test_bijection1 original_e;
  [%expect {|=test test=test test= ==  |}]

let%expect_test "bijection 1 -c" = 
  List.iter to_test_bijection1 original_c;
  [%expect {|,test test,test test, ,,  |}]

let%expect_test "bijection 1 -a" = 
  List.iter to_test_bijection1 original_a;
  [%expect {|&test test&test test& &&  |}]

let%expect_test "bijection 1 --" = 
  List.iter to_test_bijection1 original__;
  [%expect {|-test test-test test- --  |}]

let%expect_test "bijection 1 all" = 
  List.iter to_test_bijection1 original_all;
  [%expect {|=,&- =,-& =&,- =&-, =-,& =-&, ,=&- ,=-& ,&=- ,&-= ,-=& ,-&= &=,- &=-, &,=- &,-= &-=, &-,= -=,& -=&, -,=& -,&= -&=, -&,= |}]


let%expect_test "bijection 2 -e" = 
  List.iter to_test_bijection2 coded_e;
  [%expect {|-etest test-etest test-e -e-e  |}]

let%expect_test "bijection 2 -c" = 
  List.iter to_test_bijection2 coded_c;
  [%expect {|-ctest test-ctest test-c -c-c  |}]

let%expect_test "bijection 2 -a" =
  List.iter to_test_bijection2 coded_a;
  [%expect {|-atest test-atest test-a -a-a  |}]

let%expect_test "bijection 2 --" = 
  List.iter to_test_bijection2 coded__;
  [%expect {|--test test--test test-- ----  |}]

let%expect_test "bijection 2 all" = 
  List.iter to_test_bijection2 codedall;
  [%expect {|-e-c-a-- -e-c---a -e-a-c-- -e-a---c -e---c-a -e---a-c -c-e-a-- -c-e---a -c-a-e-- -c-a---e -c---e-a -c---a-e -a-e-c-- -a-e---c -a-c-e-- -a-c---e -a---e-c -a---c-e ---e-c-a ---e-a-c ---c-a-e ---c-a-e ---a-e-c ---a-c-e |}]