let test_main_function () =
  (* 假设你的 main.exe 提供了某种功能供测试 *)
  let result = Sys.command "../src/main.exe ../../../test/test.f" in
  Alcotest.(check int) "Expect 0" 0 result

let () =
  let open Alcotest in
  run "MainTests"
    [ ("basictests", [ test_case "Run main" `Quick test_main_function ]) ]
