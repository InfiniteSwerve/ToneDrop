module Note = struct
  type name = C | CS | D | DS | E | F | FS | G | GS | A | AS | B
  type note = { pitch : int; octave : int; scale_position : int option }
  type t = note

  let notes =
    [| "C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"; "A"; "A#"; "B" |]

  let eq l r = l.pitch = r.pitch && l.octave = r.octave
  let to_number note = note.pitch

  let of_number pitch octave =
    if pitch > 11 then invalid_arg "Number out of range"
    else { pitch; octave; scale_position = None }

  let of_pos position =
    let octave = position / 12 in
    let pitch = position mod 12 in
    of_number pitch octave

  let to_pos note = note.pitch + (note.octave * 12)

  let of_name (note : string) (octave : int) =
    let pitch_o = Array.find_index (fun name -> note = name) notes in
    match pitch_o with
    | Some pitch -> of_number pitch octave
    | None ->
        raise
          (failwith
             (Printf.sprintf
                "Note.of_name should always be able to find a note, but \
                 couldn't find %s"
                note))

  let to_string note = Printf.sprintf "%s%d" notes.(note.pitch) note.octave

  let transpose ?(scale_position : int option = None)
      { pitch = old_pitch; octave; _ } (operator : int) : note =
    let pitch = (old_pitch + operator) mod 12 in
    let octaveChange = (old_pitch + operator) / 12 in
    let octave = octave + octaveChange in
    { pitch; octave; scale_position }

  let transpose_notes root notes =
    List.map (fun operator -> transpose root operator) notes

  let compare n1 n2 =
    Int.compare ((n1.octave * 12) + n1.pitch) ((n2.octave * 12) + n2.pitch)

  (* Non symmetric distance. We care about going up vs down *)
  (* Going from a smaller pos to larget yields a positive number, negative otherwise *)
  let dist (from : t) (target : t) =
    (12 * (target.octave - from.octave)) + (target.pitch - from.pitch)

  let c4 = of_number 0 4
  let cs4 = of_number 1 4
  let d4 = of_number 2 4
  let ds4 = of_number 3 4
  let e4 = of_number 4 4
  let f4 = of_number 5 4
  let fs4 = of_number 6 4
  let g4 = of_number 7 4
  let gs4 = of_number 8 4
  let a4 = of_number 9 4
  let as4 = of_number 10 4
  let b4 = of_number 11 4
end

module Chord = struct
  type chord = Major | Minor | Dominant7 | Major7 | Minor7
  type t = chord

  let major_notes = [ 0; 4; 7 ]
  let minor_notes = [ 0; 3; 7 ]
  let dominant7_notes = [ 0; 4; 7; 10 ]
  let major7_notes = [ 0; 4; 7; 11 ]
  let minor7_notes = [ 0; 3; 7; 10 ]

  let spell (root : Note.t) (chord : t) : Note.t list =
    match chord with
    | Major -> Note.transpose_notes root major_notes
    | Minor -> Note.transpose_notes root minor_notes
    | Dominant7 -> Note.transpose_notes root dominant7_notes
    | Major7 -> Note.transpose_notes root major7_notes
    | Minor7 -> Note.transpose_notes root minor7_notes
end

module Scale = struct
  type direction = Up | Down
  type kind = Major | Minor | Chromatic

  (* For now just let notes be actual notes and not names, maybe redundant but easier to rely on *)
  type scale = {
    key : string;
    root : Note.t;
    notes : Note.t list;
    intervals : int list;
  }

  type t = scale

  let major_intervals = [ 0; 2; 4; 5; 7; 9; 11 ]
  let minor_intervals = [ 0; 2; 3; 5; 7; 8; 10 ]
  let chromatic_intervals = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 ]

  let get_intervals kind =
    match kind with
    | Major -> major_intervals
    | Minor -> minor_intervals
    | Chromatic -> chromatic_intervals

  let make_of_string (key : string) (intervals : int list) =
    let root = Note.of_name key 4 in
    let notes =
      List.map
        (fun interval ->
          Note.transpose ~scale_position:(Some interval) root interval)
        intervals
    in
    { key; root; intervals; notes }

  let make_of_int ?(octave = 4) (key : int) (intervals : int list) =
    let root = Note.of_number key octave in
    let notes =
      List.map
        (fun interval ->
          Note.transpose ~scale_position:(Some interval) root interval)
        intervals
    in
    { key = Note.notes.(key); root; intervals; notes }

  (* lower index = lower pos *)
  let make_big_scale scale lo ho =
    let rec go_up octave finish notes =
      let curr_octave_scale =
        make_of_int ~octave scale.root.pitch major_intervals
      in
      let new_notes =
        List.map
          (fun (note : Note.t) -> note.pitch + (note.octave * 12))
          curr_octave_scale.notes
      in
      match octave = finish with
      | true -> List.concat (new_notes :: notes)
      | false -> go_up (octave - 1) finish (new_notes :: notes)
    in
    go_up ho lo []

  (* TODO:  Normalize the target octave to be within the start octave *)
  let get_path scale (start_note : Note.t) (end_note : Note.t) : Note.t list =
    let d = Note.dist start_note end_note in
    if Note.to_pos start_note = Note.to_pos end_note then [ start_note ]
    else
      let lower, upper, direction =
        if d >= 7 then
          ({ end_note with octave = end_note.octave - 1 }, start_note, Down)
        else if d <= -7 then
          (start_note, { end_note with octave = end_note.octave + 1 }, Up)
        else if d > 0 then (start_note, end_note, Up)
        else (end_note, start_note, Down)
      in
      let lower_pos = Note.to_pos lower in
      let upper_pos = Note.to_pos upper in
      let big_scale = make_big_scale scale lower.octave upper.octave in
      let result =
        lower
        :: (List.filter
              (fun pos -> lower_pos < pos && pos < upper_pos)
              big_scale
           |> List.map Note.of_pos)
        @ [ upper ]
      in
      match direction with Down -> List.rev result | Up -> result

  let random_note (scale : scale) =
    List.nth scale.notes (Random.int (List.length scale.intervals))

  let get_note_and_path (scale : scale) =
    let note = random_note scale in
    (note, get_path scale note scale.root)
end
