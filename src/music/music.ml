module Note = struct
  type name = C | CS |  D | DS | E | F | FS | G | GS | A | AS | B 
  type note = {name: name;pitch: int; octave: int}
  type t = note

  let all_names = [| C; CS; D; DS; E; F; FS; G; GS; A; AS; B |] 
  let note_strs = [| "C"; "C#"; "D"; "D#"; "E"; "F";"F#"; "G"; "G#"; "A"; "A#"; "B" |] 

  let to_number note = 
    note.pitch

  let of_number pitch octave =
    if pitch > 11 then invalid_arg "Number out of range"
    else
      {name=all_names.(pitch); pitch; octave}

  let of_name note octave = 
    let pitch_o = Array.find_index (fun name -> note == name) all_names in 
    match pitch_o with 
    | Some pitch -> of_number pitch octave
    | None -> raise (failwith "Note.of_name should always be able to find a note")

  let to_string note = 
    Printf.sprintf "%s%d" note_strs.(note.pitch) note.octave
    

  let construct name octave = 
    of_number name octave
      
  let transpose {pitch=old_pitch;octave=octave; _} operator : note =
    let pitch = (old_pitch + operator) mod 12 in
    let octaveChange = (old_pitch + operator) / 12 in
    let name = all_names.(pitch) in
    let octave = octave + octaveChange in
    {name; pitch; octave }

  let transpose_notes root notes = 
    List.map (fun operator -> transpose root operator) notes

  let compare n1 n2 = 
    Int.compare (n1.octave * 12 + n1.pitch) (n2.octave * 12 + n2.pitch)

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

(* How do we want to create chords? A constructor function?
   Like, play <note>?
   I guess we can just represent all the chords as sets of note in one octave. We can probably also represent voicings, but not now.
   Now we just need a function that takes a note and chord type and pushes out the chord *)
end

module Chord = struct
  type chord = Major | Minor | Dominant7 | Major7 | Minor7
    type t = chord

  let major_notes = [0;4;7]
  let minor_notes = [0;3;7]
  let dominant7_notes = [0;4;7;10]
  let major7_notes = [0;4;7;11]
  let minor7_notes = [0;3;7;10]

  let spell (root: Note.t) (chord: t) = 
      match chord with
      | Major -> Note.transpose_notes root major_notes
      | Minor -> Note.transpose_notes root minor_notes
      | Dominant7 -> Note.transpose_notes root  dominant7_notes
      | Major7 -> Note.transpose_notes root major7_notes
      | Minor7 -> Note.transpose_notes root minor7_notes

end

module Scale = struct
  type kind = Major | Minor | Chromatic
  (* For now just let notes be actual notes and not names, maybe redundant but easier to rely on *)
  type t = {
    key: Note.t;
    kind: kind;
    notes: Note.t list;
  }

  let major_scale = [0;2;4;5;7;9;11;]
  let minor_scale = [0;2;3;5;7;8;10;]
  let chromatic_scale = [0;1;2;3;4;5;6;7;8;9;10;11]

  let get_intervals kind = 
    match kind with
    | Major -> major_scale
    | Minor -> minor_scale
    | Chromatic -> chromatic_scale

  let make key kind = 
    let key = Note.of_name key 4 in 
    let notes = Note.transpose_notes key (get_intervals kind) in 
    {
      key;
      kind;
      notes
    }

  (* Gives the note after the current note thats diatonic *)
  let succ (note: Note.t) scale = 
    let next_root = {(List.hd scale.notes) with octave = note.octave + 1} in 
    if note.pitch = 11 then next_root else
    let rec find (note: Note.t) (notes: Note.t list) = 
      match notes with 
        | [] -> raise (invalid_arg "scale has invalid notes")
        | [hd] -> if note.pitch < hd.pitch then hd else next_root
        | hd::(next_hd::_ as tl) ->
          if note.pitch < hd.pitch then find note tl 
          else if note.pitch == hd.pitch then next_hd
          else List.hd tl
        in
    find note scale.notes

  (* Gives the previous note in the current scale *)
  let pred (note: Note.t) scale = 
    let prev_root = {(List.hd scale.notes) with octave = note.octave - 1} in 
    let notes = List.rev scale.notes in 
    if note.pitch = 0 then prev_root else
    let rec find (note: Note.t) (notes: Note.t list) = 
      match notes with 
        | [] -> raise (invalid_arg "scale has invalid notes")
        | [hd] -> if note.pitch > hd.pitch then hd else prev_root
        | hd::(next_hd::_ as tl) ->
          if note.pitch > hd.pitch then find note tl 
          else if note.pitch == hd.pitch then next_hd
          else List.hd tl
        in
    find note notes
  


    
end
    
module Solfege = struct
  type scale = Major | Minor | Chromatic
  type note = Do | Ra | Re | Me | Mi | Fa | Fi | Sol | Le | La | Te | Ti
  type t = note

  (* let of_note (key: Note.t) (note: Note.t) *)
  (*   match note with *)
  (*     |  *)


end

module Progression = struct
  type cadence = Authentic | Plagal | TwoFiveOne

end

(* This assumes the target is diatonic to key *)
let get_path (from: Note.t) (target: Note.t) (key: Scale.t) : Note.t list = 
  (* neg from < target *)
  let rec march ?(path=[]) curr target mover  = 
    match curr = target with
    | true -> List.rev (target::path)
    | false -> march ~path:(curr::path) (mover curr key) target mover 
  in

  match Note.compare from target with
  | -1 -> march from target Scale.succ
  | 0 -> [target]
  | 1 -> march from target Scale.pred
  | _ -> raise (failwith "Note.compare should never return anything but -1, 0, or 1")

  







