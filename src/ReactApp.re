module Note = Music.Note;
module Chord = Music.Chord;
module Scale = Music.Scale;
module Play = Synth.Play;

type synthState = {
  synth: option(Synth.t),
  audioContextStarted: bool,
};

type synthCallback = Synth.t => unit;

module App = {
  [@react.component]
  // Do as little shit as possible in this file. Do pretty much all the heavy lifting in straight OCaml.
  // TODO: Play chord
  // TODO: Send whatever notes to tonic
  // tODO: Play progression
  // TODO: Do the functional ear trainer thing
  let make = () => {
    let (synth, setSynth) = React.useState(() => None);
    let (_, setAudioContextStarted) = React.useState(() => false);
    let (path, setPath) = React.useState(() => None);

    let withSynth = (synth, callback: synthCallback) => {
      switch (synth) {
      | Some(actualSynth) => callback(actualSynth)
      | None =>
        let newSynth = Synth.createPolySynth();
        setSynth(_ => Some(newSynth));
        setAudioContextStarted(_ => true);
        Js.log("Audio Started");
        callback(newSynth);
      };
    };
    let playNote = synth => {
      let note = Note.of_number(0, 4);
      Synth.play_note(synth, note);
      Js.log("Playing note");
    };

    let playChord = synth => {
      Play.chord(synth, Note.c4, Chord.Major);
      Js.log("Playing notes");
    };

    let playNoteGetPath = synth => {
      let scale = Scale.make_of_string("C", Scale.major_intervals);
      let (note, path) = Scale.get_note_and_path(scale);
      setPath(_ => Some(path));

      Play.chord(synth, scale.root, Chord.Major);
      ignore(Js.Global.setTimeout(() => {Play.note(synth, note)}, 800));
    };

    let playResolutionPath = synth => {
      switch (path) {
      | Some(actualPath) => Play.path(synth, actualPath, 300)
      | None => Js.log("Ain't no path to resolve")
      };
    };

    <div>
      <button onClick={_event => withSynth(synth, playNote)}>
        "Play Note and initialize synth"->React.string
      </button>
      <button onClick={_event => withSynth(synth, playChord)}>
        "Play Notes"->React.string
      </button>
      <button onClick={_event => withSynth(synth, playNoteGetPath)}>
        "Play Base Chord"->React.string
      </button>
      <button onClick={_event => withSynth(synth, playResolutionPath)}>
        "Play Resolution Path"->React.string
      </button>
    </div>;
  };
};

ReactDOM.querySelector("#root")
->(
    fun
    | Some(root) =>
      ReactDOM.Client.render(ReactDOM.Client.createRoot(root), <App />)
    | None =>
      Js.Console.error(
        "Failed to start React: couldn't find the #root element",
      )
  );
