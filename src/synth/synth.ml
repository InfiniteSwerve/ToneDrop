module Note = Music.Note
module Chord = Music.Chord

type synth
type t = synth
type 'a callback = 'a -> unit

(* https://melange.re/v2.2.0/build-system/#handling-assets *)
external createPolySynth : unit -> synth = "createPolySynth"
[@@mel.module "./ToneInterop.js"]

external triggerAttackRelease : synth -> string -> string -> unit
  = "triggerAttackRelease"
[@@mel.module "./ToneInterop.js"]

external triggerListAttackRelease : synth -> string array -> string -> unit
  = "triggerAttackRelease"
[@@mel.module "./ToneInterop.js"]

external schedule : 'a callback -> string -> unit = "schedule"
[@@mel.module "./ToneInterop.js"]

external startTransport : unit -> unit = "startTransport"
[@@mel.module "./ToneInterop.js"]

external stopTransport : unit -> unit = "stopTransport"
[@@mel.module "./ToneInterop.js"]

external clearTransport : unit -> unit = "clearTransport"
[@@mel.module "./ToneInterop.js"]

external releaseAll : synth -> unit = "releaseAll"
[@@mel.module "./ToneInterop.js"]

let play_note (synth : synth) (note : Note.t) : unit =
  let note_str = Note.to_string note in
  triggerAttackRelease synth note_str "8n"

(* I guess js lists are the same as ocaml arrays in memory? *)
let play_notes (synth : synth) (chord : Chord.t) : unit =
  let notes = chord.notes in
  let note_strings =
    List.map (fun note -> Note.to_string note) notes |> Array.of_list
  in
  triggerListAttackRelease synth note_strings "8n"

let drop_audio synth =
  releaseAll synth;
  clearTransport ()

let highlight_note
    (set_highlight_notes : (([> `None ] as 'a) array -> 'a array) -> unit)
    (note : Note.t) start (duration : float) highlight (root : Note.t) =
  schedule
    (fun () ->
      set_highlight_notes (fun highlight_notes ->
          let new_highlights = Array.copy highlight_notes in
          new_highlights.(note.pitch - root.pitch + (note.octave / 5 * 12)) <-
            highlight;
          new_highlights))
    (Printf.sprintf "+%f" start);
  schedule
    (fun () ->
      set_highlight_notes (fun highlight_notes ->
          let new_highlights = Array.copy highlight_notes in
          new_highlights.(note.pitch - root.pitch + (note.octave / 5 * 12)) <-
            `None;
          new_highlights))
    (Printf.sprintf "+%f" (start +. duration))

module Play = struct
  let note (synth : synth) (note : Note.t) : unit = play_note synth note
  let chord (synth : synth) (chord : Chord.t) : unit = play_notes synth chord

  let path (synth : synth) set_highlight_notes (notes : Note.t list) highlight
      root (_delay : int) =
    drop_audio synth;
    set_highlight_notes (fun _ -> Array.make 13 `None);
    List.iteri
      (fun i (n : Note.t) ->
        highlight_note set_highlight_notes n
          (float_of_int i *. 0.75)
          0.75 highlight root;

        schedule
          (fun () -> note synth n)
          (Printf.sprintf "+%f" (float_of_int i *. 0.75)))
      notes;
    startTransport ()

  let chords (synth : synth) set_highlight_notes (_root : Note.t)
      (chords : Chord.t list) =
    drop_audio synth;
    set_highlight_notes (fun _ -> Array.make 13 `None);
    List.iteri
      (fun i n ->
        schedule
          (fun () -> chord synth n)
          (Printf.sprintf "+%f" (float_of_int i *. 0.75)))
      chords;
    startTransport ()

  let chords_with_callback (synth : synth) set_highlight_notes (_root : Note.t)
      (chords : Chord.t list) callback : unit =
    drop_audio synth;
    set_highlight_notes (fun _ -> Array.make 13 `None);
    List.iteri
      (fun i n ->
        schedule
          (fun () -> chord synth n)
          (Printf.sprintf "+%f" (float_of_int i *. 0.75)))
      chords;
    schedule callback
      (Printf.sprintf "+%f"
         (List.length chords |> float_of_int |> fun x -> x *. 0.85));
    startTransport ()
end
