module Note = Music.Note

type synth

external createPolySynth : unit -> synth = "createPolySynth"
  [@@mel.module "../../../../../../src/world/ToneInterop.js"] 
  (* [@@mel.module "ToneInterop.js"]  *)

external triggerAttackRelease : synth -> string -> string -> unit = "triggerAttackRelease"
  [@@mel.send]
 
external triggerListAttackRelease : synth -> string array -> string -> unit = "triggerAttackRelease"
  [@@mel.send]

let play_note (synth: synth) (note: Note.t) : unit = 
  let note_str = Note.to_string note in 
  triggerAttackRelease synth note_str "8n"

  (* I guess js lists are the same as ocaml arrays in memory? *)
let play_notes (synth: synth) (notes: Note.t list): unit = 
  let note_strings = List.map (fun note -> Note.to_string note) notes
    |> Array.of_list
  in
  triggerListAttackRelease synth note_strings "8n"
