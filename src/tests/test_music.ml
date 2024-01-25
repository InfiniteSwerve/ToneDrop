open Music

let c_maj = Scale.make Note.C Scale.Major 

let test_successor () = 

  let succ_c4 = Scale.succ Note.c4 c_maj in 
  Alcotest.(check (pair int int)) "C4 -> D4" (2, 4) (succ_c4.pitch, succ_c4.octave);

  let succ_b4 = Scale.succ Note.c4 c_maj in 
  Alcotest.(check (pair int int)) "B4 -> C5" (0, 5) (succ_b4.pitch, succ_b4.octave)
  

let test_path () = 
  let c_d = get_path Note.c4 Note.d4 c_maj in 
  (* let c_c= get_path Note.c4 Note.(of_number 0 5) c_maj in  *)
  
  Alcotest.(check (list int)) "C -> D = 0 -> 2" [0;2] (List.map Note.to_number c_d)
  (* Alcotest.(check (list int)) "C -> C = 0 -> 2 -> 4 -> 5 -> 7 -> 9 -> 11" [0;2;4;5;7;9;11] (List.map Note.to_number c_c) *)

let test_suite = [
  "successor", `Quick, test_successor;
  "get_path", `Quick, test_path;
]

let () =
  let open Alcotest in
  run "My Tests" [
    "arithmetic", test_suite;
  ]
