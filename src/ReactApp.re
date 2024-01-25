module Note = Music.Note;
module App = {
  [@react.component]
  let make = () => {
    let (synth, setSynth) = React.useState(() => None);
    let (audioContextStarted, setAudioContextStarted) =
      React.useState(() => false);

    let playNote = () => {
      let note = Note.of_number(0, 4);
      switch (synth) {
      | Some(actualSynth) =>
        Synth.play_note(actualSynth, note);
        Js.log("Playing note");
      | None => Js.log("Synth not initialized")
      };
    };

    let playNotes = () => {
      let note1 = Note.of_number(0, 4);
      let note2 = Note.of_number(4, 4);
      let note3 = Note.of_number(7, 4);
      let notes = [note1, note2, note3];
      switch (synth) {
      | Some(actualSynth) =>
        Synth.play_notes(actualSynth, notes);
        Js.log("Playing notes");
      | None => Js.log("Synth not initialized")
      };
    };
    let startAudio = () => {
      // This function is called in response to a user action
      let synth = Some(Synth.createPolySynth());
      setSynth(_ => synth);
      setAudioContextStarted(_ => true); // Update state to indicate that audio context has started
      Js.log("Audio Started");
    };

    <div>
      {audioContextStarted
         ? <div> {React.string("Audio Ready")} </div>
         : <button onClick={_event => startAudio()}>
             "Start Audio"->React.string
           </button>}
      <button onClick={_event => playNote()}>
        "Play Note"->React.string
      </button>
      <button onClick={_event => playNotes()}>
        "Play Notes"->React.string
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
