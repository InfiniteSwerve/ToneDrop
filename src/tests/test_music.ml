open Music

let c_maj = Scale.of_string "C" Scale.major_intervals
let g_maj = Scale.of_string "G" Scale.major_intervals

let test_dist () =
  let n = Note.transpose Note.c4 2 in
  Alcotest.(check int) "C4 -> D4" 2 (Note.dist Note.c4 n)

let test_transpose () =
  let n = Note.transpose Note.c4 2 in
  Alcotest.(check (pair int int)) "Transpose C4 -> D4" (2, 4) (n.pitch, n.octave);
  let n = Note.transpose Note.c4 12 in
  Alcotest.(check (pair int int)) "Transpose C4 -> C5" (0, 5) (n.pitch, n.octave)

let test_make_big_scale () =
  let target =
    List.map
      (fun octave ->
        List.map (fun pitch -> pitch + (octave * 12)) Scale.major_intervals)
      [ -1; 0; 1 ]
    |> List.concat
  in
  let big_scale = Scale.make_big_scale c_maj 0 1 in
  Alcotest.(check (list int)) "Big Scale C major 0 -> 1" target big_scale;
  let target =
    List.map
      (fun octave ->
        List.map (fun pitch -> pitch + (octave * 12)) Scale.major_intervals)
      [ -1; 0 ]
    |> List.concat
  in
  let big_scale = Scale.make_big_scale c_maj 0 0 in
  Alcotest.(check (list int)) "Big Scale C major 0 -> 0" target big_scale

let test_path () =
  let c_d =
    Scale.get_path c_maj (Note.of_number 2 0) (Note.of_number 0 0)
    |> List.map Note.to_pos
  in
  Alcotest.(check (list int)) "Get Path C0 -> D0 = " [ 2; 0 ] c_d;

  let c_c =
    Scale.get_path c_maj (Note.of_number 1 1) Note.(of_number 0 0)
    |> List.map Note.to_pos
  in
  Alcotest.(check (list int)) "Get Path C0 -> C#1" [ 13; 12 ] c_c;

  let g_c =
    Scale.get_path c_maj (Note.of_number 7 0) Note.(of_number 0 0)
    |> List.map Note.to_pos
  in
  Alcotest.(check (list int)) "Get Path G0 -> C0" [ 7; 9; 11; 12 ] g_c;

  let b_c =
    Scale.get_path c_maj (Note.of_number 11 0) Note.(of_number 0 0)
    |> List.map Note.to_pos
  in
  Alcotest.(check (list int)) "Get Path G0 -> C0" [ 11; 12 ] b_c;

  let d_g =
    Scale.get_path g_maj (Note.of_number 2 5) Note.(of_number 7 4)
    |> List.map Note.to_pos
  in
  let off = 12 * 5 in
  Alcotest.(check (list int))
    "Get Path D5 -> G5 G maj"
    [ 2 + off; 4 + off; 6 + off; 7 + off ]
    d_g;

  let d_g =
    Scale.get_path g_maj (Note.of_number 2 0) Note.(of_number 7 0)
    |> List.map Note.to_pos
  in
  let off = 0 in
  Alcotest.(check (list int))
    "Get Path D0 -> G0 G maj"
    [ 2 + off; 4 + off; 6 + off; 7 + off ]
    d_g

let test_scale_swap () =
  let dumb_scale = Scale.of_note Note.c4 [ 0 ] in
  let swapped_scale = Scale.swap dumb_scale 2 in
  Alcotest.(check (list int)) "0 -> 0 2" [ 0; 2 ] swapped_scale.intervals;

  let swapped_scale = Scale.swap swapped_scale 2 in
  Alcotest.(check (list int)) "0 2 -> 0" [ 0 ] swapped_scale.intervals;

  let scale = Scale.of_note Note.c4 [ 0; 11 ] in
  let swapped_scale = Scale.swap scale 8 in
  Alcotest.(check (list int))
    "0 11 -> 0 8 11" [ 0; 8; 11 ] swapped_scale.intervals;

  let scale = Scale.of_note Note.c4 [ 0; 2; 4; 5; 7; 9; 11 ] in
  let swapped_scale = Scale.swap scale 0 in
  Alcotest.(check (list int))
    "2 - 11 major" [ 2; 4; 5; 7; 9; 11 ] swapped_scale.intervals;
  let swapped_scale = Scale.swap swapped_scale 0 in
  Alcotest.(check (list int))
    "0 - 11 major" [ 0; 2; 4; 5; 7; 9; 11 ] swapped_scale.intervals

let test_suite =
  [
    ("dist", `Quick, test_dist);
    ("transpose", `Quick, test_transpose);
    ("make_big_scale", `Quick, test_make_big_scale);
    ("get_path", `Quick, test_path);
    ("swap", `Quick, test_scale_swap);
  ]

let () =
  let open Alcotest in
  run "My Tests" [ ("music", test_suite) ]
