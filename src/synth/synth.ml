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

external changeBPM : int -> unit = "changeBPM" [@@mel.module "./ToneInterop.js"]

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
  stopTransport ();
  clearTransport ()

type highlight = Correct | Incorrect | Blank

let highlight_note
    (set_highlight_notes : (highlight array -> highlight array) -> unit)
    (note : Note.t) start (duration : float) (highlight : highlight)
    (root : Note.t) =

  let highlight_position = 
    let diff = note.pitch - root.pitch + (note.octave - root.octave) * 12 in
    if diff = 12 then 12
    else (diff + 120) mod 12
  in

  schedule
    (fun () ->
      set_highlight_notes (fun highlight_notes ->
          let new_highlights = Array.copy highlight_notes in
          new_highlights.(highlight_position) <- highlight;
          new_highlights))
    (Printf.sprintf "+%f" start);
  schedule
    (fun () ->
      set_highlight_notes (fun highlight_notes ->
          let new_highlights = Array.copy highlight_notes in
          new_highlights.(highlight_position) <- Blank;
          new_highlights))
    (Printf.sprintf "+%f" (start +. duration))

module Play = struct
  let note (synth : synth) (note : Note.t) : unit = play_note synth note
  let chord (synth : synth) (chord : Chord.t) : unit = play_notes synth chord

  let path (synth : synth)
      (set_highlight_notes : (highlight array -> highlight array) -> unit)
      (notes : Note.t list) (highlight : highlight) (root : Note.t) (bpm : int)
      =
    drop_audio synth;
    set_highlight_notes (fun _ -> Array.make 13 Blank);

    let beatDuration = 60. /. float_of_int bpm in
    let otherNoteDuration = beatDuration *. 0.5 in

    List.iteri
      (fun i (n : Note.t) ->
        let noteDuration = if i == 0 then beatDuration else otherNoteDuration in
        let startTime =
          if i == 0 then 0.
          else (float_of_int (i - 1) *. otherNoteDuration) +. beatDuration
        in

        highlight_note set_highlight_notes n startTime noteDuration highlight
          root;

        schedule (fun () -> note synth n) (Printf.sprintf "+%f" startTime))
      notes;
    startTransport ()

  let chords (synth : synth) setProgressionIndex
      (set_highlight_notes : (highlight array -> highlight array) -> unit)
      (chords : Chord.t list) bpm =
    drop_audio synth;
    set_highlight_notes (fun _ -> Array.make 13 Blank);
    let duration = 0.75 /. float_of_int (bpm / 60) in
    List.iteri
      (fun i n ->
        schedule
          (fun () ->
            setProgressionIndex (fun _ -> i);
            chord synth n)
          (Printf.sprintf "+%f" (float_of_int i *. duration)))
      chords;
    startTransport ()

  let chords_with_callback (synth : synth) set_highlight_notes (_root : Note.t)
      (chords : Chord.t list) bpm callback : unit =
    (* drop_audio synth; *)
    set_highlight_notes (fun _ -> Array.make 13 Blank);
    let duration = 0.75 /. float_of_int (bpm / 60) in
    List.iteri
      (fun i n ->
        schedule
          (fun () -> chord synth n)
          (Printf.sprintf "+%f" (float_of_int i *. duration)))
      chords;
    schedule callback
      (Printf.sprintf "+%f"
         (List.length chords |> float_of_int |> fun x -> x *. 1.15 *. duration));
    startTransport ()
end
