open Music

let c_maj = Scale.make_of_string "C" Scale.major_intervals

let test_dist () =
  let n = Note.transpose Note.c4 2 in
  Alcotest.(check int) "C4 -> D4" 2 (Note.dist Note.c4 n)

let test_transpose () =
  let n = Note.transpose Note.c4 2 in
  Alcotest.(check (pair int int)) "C4 -> D4" (2, 4) (n.pitch, n.octave);
  let n = Note.transpose Note.c4 12 in
  Alcotest.(check (pair int int)) "C4 -> C5" (0, 5) (n.pitch, n.octave)

let test_make_big_scale () =
  let target =
    List.map
      (fun octave ->
        List.map (fun pitch -> pitch + (octave * 12)) Scale.major_intervals)
      [ 0; 1 ]
    |> List.concat
  in
  let big_scale = Scale.make_big_scale c_maj 0 1 in
  Alcotest.(check (list int)) "C major 0 -> 1" target big_scale;
  let target =
    List.map
      (fun octave ->
        List.map (fun pitch -> pitch + (octave * 12)) Scale.major_intervals)
      [ 0 ]
    |> List.concat
  in
  let big_scale = Scale.make_big_scale c_maj 0 0 in
  Alcotest.(check (list int)) "C major 0 -> 0" target big_scale

let test_path () =
  let c_d =
    Scale.get_path c_maj (Note.of_number 2 0) (Note.of_number 0 0)
    |> List.map Note.to_pos
  in
  Alcotest.(check (list int)) "C -> D = 0 -> 2" [ 2; 0 ] c_d;

  let c_c =
    Scale.get_path c_maj (Note.of_number 1 1) Note.(of_number 0 0)
    |> List.map Note.to_pos
  in
  Alcotest.(check (list int)) "C0 -> C#1" [ 13; 12 ] c_c;

  let g_c =
    Scale.get_path c_maj (Note.of_number 7 0) Note.(of_number 0 0)
    |> List.map Note.to_pos
  in
  Alcotest.(check (list int)) "G0 -> C0" [ 7; 9; 11; 12 ] g_c;

  let b_c =
    Scale.get_path c_maj (Note.of_number 11 0) Note.(of_number 0 0)
    |> List.map Note.to_pos
  in
  Alcotest.(check (list int)) "G0 -> C0" [ 11; 12 ] b_c

let test_suite =
  [
    ("dist", `Quick, test_dist);
    ("transpose", `Quick, test_transpose);
    ("make_big_scale", `Quick, test_make_big_scale);
    ("get_path", `Quick, test_path);
  ]

let () =
  let open Alcotest in
  run "My Tests" [ ("music", test_suite) ]
