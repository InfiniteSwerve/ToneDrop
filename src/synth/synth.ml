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

let play_note (synth : synth) (note : Note.t) : unit =
  let note_str = Note.to_string note in
  triggerAttackRelease synth note_str "8n"

(* I guess js lists are the same as ocaml arrays in memory? *)
let play_notes (synth : synth) (notes : Note.t list) : unit =
  let note_strings =
    List.map (fun note -> Note.to_string note) notes |> Array.of_list
  in
  triggerListAttackRelease synth note_strings "8n"

module Play = struct
  let note (synth : synth) (note : Note.t) : unit = play_note synth note

  let chord (synth : synth) (root : Note.t) (kind : Chord.t) : unit =
    let chord = Chord.spell root kind in
    play_notes synth chord

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
end
