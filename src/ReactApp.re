module Note = Music.Note;
module Chord = Music.Chord;
module Scale = Music.Scale;
module Play = Synth.Play;

type synthCallback = Synth.t => unit;

module App = {
  [@react.component]
  // Do as little shit as possible in this file. Do pretty much all the heavy lifting in straight OCaml.
  // TODO: Play progression
  // TODO: Visualization of chord relative to key via p5.js
  // TODO: Do the functional ear trainer thing
  // TODO: ToneDrop logo in the top left
  //
  let make = () => {
    let (synth, setSynth) = React.useState(() => None);
    let (_, setAudioContextStarted) = React.useState(() => false);
    let (path, setPath) = React.useState(() => None);
    let (key, _setKey) = React.useState(() => Note.c4);
    let (keyChangeOpen, setKeyChangeOpen) = React.useState(() => false);
    let (guessNote, setGuessNote) = React.useState(() => None);

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

    let playNoteGetPath = synth => {
      let scale = Scale.of_note(key, Scale.major_intervals);
      let (note, path) = Scale.get_note_and_path(scale);
      setGuessNote(_ => Some(note));
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

    let makeButton =
        (~noteValue, ~label, ~handleClick, ~accidental, ~gridColumn) => {
      let className = accidental ? "note-button sharp-flat" : "note-button";
      let gridColumn = Printf.sprintf("%d / span 2", gridColumn);
      <button
        className
        onClick={_event => handleClick(noteValue)}
        style={ReactDOM.Style.make(~gridColumn, ())}>
        label->React.string
      </button>;
    };
    let handleButtonPress = button_value => {
      let local_note = Note.transpose(key, button_value);
      withSynth(synth, _synth => Play.note(_synth, local_note));
      let local_note = Some(local_note);
      let _ =
        Js.Global.setTimeout(
          () =>
            if (Option.equal(Note.eq, local_note, guessNote)) {
              withSynth(synth, playResolutionPath);
            },
          300,
        );
      ();
    };
    <div className="app-container">
      <div className="sidebar">
        <button
          className="sidebar-button"
          onClick={_event => setKeyChangeOpen(prev => !prev)}>
          {Note.to_string(key)->React.string}
        </button>
        {keyChangeOpen
           ? <div className="dropdown-content">
               <div onClick={_event => _setKey(_ => Note.of_name("C", 4))}>
                 "C"->React.string
               </div>
               <div onClick={_event => _setKey(_ => Note.of_name("C#", 4))}>
                 "C#/Db"->React.string
               </div>
               <div onClick={_event => _setKey(_ => Note.of_name("D", 4))}>
                 "D"->React.string
               </div>
               <div onClick={_event => _setKey(_ => Note.of_name("D#", 4))}>
                 "D#/Eb"->React.string
               </div>
               <div onClick={_event => _setKey(_ => Note.of_name("E", 4))}>
                 "E"->React.string
               </div>
               <div onClick={_event => _setKey(_ => Note.of_name("F", 4))}>
                 "F"->React.string
               </div>
               <div onClick={_event => _setKey(_ => Note.of_name("F#", 4))}>
                 "F#/Gb"->React.string
               </div>
               <div onClick={_event => _setKey(_ => Note.of_name("G", 4))}>
                 "G"->React.string
               </div>
               <div onClick={_event => _setKey(_ => Note.of_name("G#", 4))}>
                 "G#/Ab"->React.string
               </div>
               <div onClick={_event => _setKey(_ => Note.of_name("A", 4))}>
                 "A"->React.string
               </div>
               <div onClick={_event => _setKey(_ => Note.of_name("A#", 4))}>
                 "A#/Bb"->React.string
               </div>
               <div onClick={_event => _setKey(_ => Note.of_name("B", 4))}>
                 "B"->React.string
               </div>
             </div>
           : React.null}
      </div>
      <div className="main-content">
        <div className="button-container">
          <div className="note-grid">
            // TODO: Global time value for speeding up/slowing down
            // TODO: highlight button pressed
            // TODO: Flash button during path playing

              {makeButton(
                 ~noteValue=1,
                 ~label="Ra",
                 ~handleClick=handleButtonPress,
                 ~accidental=true,
                 ~gridColumn=1,
               )}
              {makeButton(
                 ~noteValue=3,
                 ~label="Me",
                 ~handleClick=handleButtonPress,
                 ~accidental=true,
                 ~gridColumn=3,
               )}
              {makeButton(
                 ~noteValue=6,
                 ~label="Fi",
                 ~handleClick=handleButtonPress,
                 ~accidental=true,
                 ~gridColumn=7,
               )}
              {makeButton(
                 ~noteValue=8,
                 ~label="Le",
                 ~handleClick=handleButtonPress,
                 ~accidental=true,
                 ~gridColumn=9,
               )}
              {makeButton(
                 ~noteValue=10,
                 ~label="Te",
                 ~handleClick=handleButtonPress,
                 ~accidental=true,
                 ~gridColumn=11,
               )}
            </div>
          <div className="note-grid">
            {makeButton(
               ~noteValue=0,
               ~label="Do",
               ~handleClick=handleButtonPress,
               ~accidental=false,
               ~gridColumn=1,
             )}
            {makeButton(
               ~noteValue=2,
               ~label="Re",
               ~handleClick=handleButtonPress,
               ~accidental=false,
               ~gridColumn=3,
             )}
            {makeButton(
               ~noteValue=4,
               ~label="Mi",
               ~handleClick=handleButtonPress,
               ~accidental=false,
               ~gridColumn=5,
             )}
            {makeButton(
               ~noteValue=5,
               ~label="Fa",
               ~handleClick=handleButtonPress,
               ~accidental=false,
               ~gridColumn=7,
             )}
            {makeButton(
               ~noteValue=7,
               ~label="So",
               ~handleClick=handleButtonPress,
               ~accidental=false,
               ~gridColumn=9,
             )}
            {makeButton(
               ~noteValue=9,
               ~label="La",
               ~handleClick=handleButtonPress,
               ~accidental=false,
               ~gridColumn=11,
             )}
            {makeButton(
               ~noteValue=11,
               ~label="Ti",
               ~handleClick=handleButtonPress,
               ~accidental=false,
               ~gridColumn=13,
             )}
            {makeButton(
               ~noteValue=12,
               ~label="Do",
               ~handleClick=handleButtonPress,
               ~accidental=false,
               ~gridColumn=15,
             )}
          </div>
          <div className="buttons-below-grid">
            <button
              className="function-button"
              id="repeat-question"
              onClick={_event => withSynth(synth, playNoteGetPath)}>
              "Repeat the Question"->React.string
            </button>
            <button
              className="function-button"
              id="play-answer"
              onClick={_event => withSynth(synth, playResolutionPath)}>
              "Play the Correct Answer"->React.string
            </button>
          </div>
        </div>
      </div>
    </div>;
    // TODO: Button for selecting scale notes
    // TODO: Something that learns how good you're getting at guessing and targets stuff you're bad at
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
