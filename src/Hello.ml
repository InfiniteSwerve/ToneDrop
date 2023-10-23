type synth

external new_synth : unit -> synth = "PolySynth"
[@@mel.module "tone"] [@@mel.new]

external to_destination : synth -> unit = "toDestination" [@@mel.send.pipe: synth]

let synth =
  let synth = new_synth () in
  to_destination synth;
  synth

