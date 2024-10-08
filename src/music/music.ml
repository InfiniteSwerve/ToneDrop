let list_to_string (f : 'a -> string) (l : 'a list) : string =
  "[" ^ String.concat "; " (List.map f l) ^ "]"

module Note = struct
  type name = C | CS | D | DS | E | F | FS | G | GS | A | AS | B
  type note = { pitch : int; octave : int; scale_position : int option }
  type t = note

  let notes =
    [| "C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"; "A"; "A#"; "B" |]

  let solfege =
    [| "Do"; "Ra"; "Re"; "Me"; "Mi"; "Fa"; "Fi"; "So"; "Le"; "La"; "Te"; "Ti" |]

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
  let of_string note =
    let note_length = String.length note in
    let rec find_note_index i =
      if i >= Array.length notes then
        None
      else if String.sub note 0 (note_length - 1) = notes.(i) then
        Some i
      else
        find_note_index (i + 1)
    in
    match find_note_index 0 with
    | None -> None
    | Some pitch ->
      let octave = int_of_string (String.sub note (note_length - 1) 1) in
      Some { pitch; octave; scale_position = None }
  let of_string_exn note = 
    match of_string note with
        | Some note -> note
        | None -> raise (failwith "String is not a well formed note")


  let to_name note = notes.(note.pitch)
  let to_solfege note = solfege.(note.pitch)

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
  type kind = Major | Minor | Dominant7 | Major7 | Minor7 | Complicated
  type chord = { root : Note.t; kind: kind; notes : Note.t list }
  type t = chord

  let major_notes = [ 0; 4; 7 ]
  let minor_notes = [ 0; 3; 7 ]
  let dominant7_notes = [ 0; 4; 7; 10 ]
  let major7_notes = [ 0; 4; 7; 11 ]
  let minor7_notes = [ 0; 3; 7; 10 ]

  let kinds= [Major ; Minor ; Dominant7 ; Major7 ; Minor7]
  let string_kinds= ["Major" ; "Minor" ; "Dominant7" ; "Major7" ; "Minor7"]

  let kind_to_string kind = 
      match kind with
      | Major -> "Major"
      | Minor -> "Minor"
      | Dominant7 -> "Dominant7"
      | Major7 -> "Major7"
      | Minor7 -> "Minor7"
      | Complicated -> "Complicated"

  let kind_of_string string = 
      match string with
      | "Major" -> Major
      | "Minor" -> Minor
      | "Dominant7" -> Dominant7
      | "Major7" -> Major7
      | "Minor7" -> Minor7
      |_ -> raise (failwith "Unvalid chord kind")

  let of_kind (root : Note.t) (chord : kind) : chord =
    let notes =
      match chord with
      | Major -> Note.transpose_notes root major_notes
      | Minor -> Note.transpose_notes root minor_notes
      | Dominant7 -> Note.transpose_notes root dominant7_notes
      | Major7 -> Note.transpose_notes root major7_notes
      | Minor7 -> Note.transpose_notes root minor7_notes
      | Complicated -> raise (failwith "Complicated is not a valid kind to construct a chord from")
    in
    { root; notes; kind=chord}

  let of_interval_kind (root : Note.t) (interval : int) (chord : kind) : chord =
    let new_root = Note.transpose root interval in
    of_kind new_root chord

  let of_notes root notes = { root; notes; kind=Complicated }

  let to_string chord =
    "{ root = " ^ Note.to_string chord.root ^ "\n" ^ "; notes = "
    ^ list_to_string Note.to_string chord.notes
    ^ "\n}"

  let get_random_note (chord: chord) : Note.t = List.nth chord.notes (Random.int (List.length chord.notes))

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
  let dorian_intervals = [ 0; 2; 3; 5; 7; 9; 10 ]
  let phrygian_intervals = [ 0; 1; 3; 5; 7; 8; 10 ]
  let lydian_intervals = [ 0; 2; 4; 6; 7; 9; 11 ]
  let mixolydian_intervals = [ 0; 2; 4; 5; 7; 9; 10 ]
  let locrian_intervals = [ 0; 1; 3; 5; 6; 8; 10 ]

  let get_intervals kind =
    match kind with
    | Major -> major_intervals
    | Minor -> minor_intervals
    | Chromatic -> chromatic_intervals

  let of_string (key : string) (intervals : int list) =
    let root = Note.of_name key 4 in
    let notes =
      List.map
        (fun interval ->
          Note.transpose ~scale_position:(Some interval) root interval)
        intervals
    in
    { key; root; intervals; notes }

  let to_string (s : scale) : string =
    "{ root = " ^ Note.to_string s.root ^ "\n" ^ "; notes = "
    ^ list_to_string Note.to_string s.notes
    ^ "\n" ^ "; intervals = "
    ^ list_to_string string_of_int s.intervals
    ^ " }"

  let of_note root intervals =
    let key = Note.to_name root in
    let notes =
      List.map
        (fun interval ->
          Note.transpose ~scale_position:(Some interval) root interval)
        intervals
    in
    { key; root; intervals; notes }

  let of_int ?(octave = 4) (key : int) (intervals : int list) =
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
    let rec go_up finish octave notes =
      let curr_octave_scale = of_int ~octave scale.root.pitch scale.intervals in
      let new_notes = List.map Note.to_pos curr_octave_scale.notes in
      match octave = finish with
      | true -> List.concat (new_notes :: notes)
      | false -> go_up finish (octave - 1) (new_notes :: notes)
    in
    go_up (lo - 1) ho []

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

  let of_active_notes (root : Note.t) (active_notes : bool Array.t) : scale =
    let len = Array.length active_notes - 2 in
    let rec walk curr acc =
      match curr >= 0 with
      | true -> (
          match active_notes.(curr) with
          | true -> walk (curr - 1) (curr :: acc)
          | false -> walk (curr - 1) acc)
      | false -> acc
    in
    of_note root (walk len [])

  let mem (scale : scale) (interval : int) : bool =
    List.mem interval scale.intervals

  let add (scale : scale) (interval : int) : scale =
    let rec walk l r : int list =
      match r with
      | hd :: tl -> (
          if hd == interval then scale.intervals
          else
            match hd < interval with
            | true -> walk (hd :: l) tl
            | false -> List.rev (interval :: l) @ r)
      | [] -> List.rev (interval :: l)
    in
    of_note scale.root (walk [] scale.intervals)

  let remove (scale : scale) (interval : int) : scale =
    of_note scale.root (List.filter (fun n -> n != interval) scale.intervals)

  let swap (scale : scale) (interval : int) : scale =
    match mem scale interval with
    | true -> remove scale interval
    | false -> add scale interval

  let random_scale scale = of_string Note.notes.(Random.int 12) scale.intervals

  let to_chord (scale : scale) (interval : int) (chord_size : int) =
    let root = Note.transpose scale.root interval in
    let big_scale =
      make_big_scale scale scale.root.octave (scale.root.octave + 2)
      |> List.filter (fun note -> Note.(note >= to_pos root))
    in
    let rec walk scale size acc =
      match scale with
      | _hd :: note :: tl when size < chord_size ->
          walk tl (size + 1) (note :: acc)
      | _ -> List.rev acc
    in
    let root_pos = Note.to_pos root in
    let notes = walk big_scale 0 [] |> List.map (fun note -> note - root_pos) in

    Printf.printf "%s\n" (list_to_string string_of_int notes);
    Chord.{ root; notes = Note.transpose_notes root notes; kind=Complicated }
end

module GuessableNotes = struct
  type guessableNotes = bool Array.t
  type t = guessableNotes

  let of_scale (scale : Scale.t) : guessableNotes =
    let notes = Array.make 13 false in
    let intervals = scale.intervals in
    (match List.hd intervals == 0 with
    | true ->
        notes.(0) <- true;
        notes.(12) <- true
    | false -> ());
    let rec make intervals =
      match intervals with
      | hd :: tl ->
          notes.(hd) <- true;
          make tl
      | [] -> ()
    in
    make (List.tl intervals);
    notes

  let swap (notes : guessableNotes) (note : int) =
    notes.(note) <- not notes.(note);
    notes

  let get_random_note (scale : Scale.t) (notes : guessableNotes) : Note.t =
    let note_list_b = Array.to_list notes in
    let guessable_notes =
      List.mapi (fun i b -> if b then Some i else None) note_list_b
      |> List.filter_map Fun.id
    in
    let interval =
      List.nth guessable_notes (Random.int (List.length guessable_notes))
    in
    Note.transpose scale.root interval
end

module Progression = struct
  type progression = Chord.t Array.t
  type t = progression
  let make () = 
    Array.make 1 (Chord.of_kind Note.c4 Chord.Major)

  let add progression chord = 
    Array.append progression [|chord|]

  let remove progression index =
    Array.of_list (List.filteri (fun i _ -> i != index) (Array.to_list progression))

  let swap progression index new_chord = 
    progression.(index) <- new_chord;
    progression
    
  let length progression = 
    Array.length progression

  let get progression id = 
    progression.(id)

end
