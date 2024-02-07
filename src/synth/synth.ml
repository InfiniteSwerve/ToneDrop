module Note = Music.Note
module Chord = Music.Chord

type synth
type t = synth

(* https://melange.re/v2.2.0/build-system/#handling-assets *)
external createPolySynth : unit -> synth = "createPolySynth"
[@@mel.module "./ToneInterop.js"]

external triggerAttackRelease : synth -> string -> string -> unit
  = "triggerAttackRelease"
[@@mel.send]

external triggerListAttackRelease : synth -> string array -> string -> unit
  = "triggerAttackRelease"
[@@mel.send]

external releaseAll : synth -> unit = "releaseAll" [@@mel.send]

let play_note (synth : synth) (note : Note.t) : unit =
  Printf.printf "releasing all notes\n";
  releaseAll synth;
  let note_str = Note.to_string note in
  Printf.printf "Playing note\n";
  triggerAttackRelease synth note_str "8n"

(* I guess js lists are the same as ocaml arrays in memory? *)
let play_notes (synth : synth) (chord : Chord.t) : unit =
  Printf.printf "releasing all notes\n";
  releaseAll synth;
  let notes = chord.notes in
  Printf.printf "Playing notes\n";
  let note_strings =
    List.map (fun note -> Note.to_string note) notes |> Array.of_list
  in
  triggerListAttackRelease synth note_strings "8n"

module Play = struct
  let note (synth : synth) (note : Note.t) : unit = play_note synth note
  let chord (synth : synth) (chord : Chord.t) : unit = play_notes synth chord

  let path (synth : synth) (notes : Note.t list) (delay : int) =
    let rec walk notes =
      match notes with
      | note :: rest ->
          ignore
            (Js.Global.setTimeout
               (fun () ->
                 play_note synth note;
                 walk rest)
               delay)
      | [] -> ()
    in
    walk (List.tl notes)

  let chords (synth : synth) (_root : Note.t) (chords : Chord.t list)
      (delay : int) : unit =
    let rec walk chords =
      match chords with
      | chord :: rest ->
          play_notes synth chord;
          ignore (Js.Global.setTimeout (fun () -> walk rest) delay)
      | [] -> ()
    in
    walk chords

  let chords_with_callback (synth : synth) (_root : Note.t)
      (chords : Chord.t list) (delay : int) callback : unit =
    let rec walk chords =
      match chords with
      | chord :: rest ->
          play_notes synth chord;
          ignore (Js.Global.setTimeout (fun () -> walk rest) delay)
      | [] -> callback ()
    in
    walk chords
end
