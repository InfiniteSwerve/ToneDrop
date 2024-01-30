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
  // TODO: Play progression
  // TODO: Visualization of chord relative to key via p5.js
  // TODO: Resolve to root when correct note is pressed
  // TODO: Do the functional ear trainer thing
  // TODO: ToneDrop logo in the top left
  //
  let make = () => {
    let (synth, setSynth) = React.useState(() => None);
    let (_, setAudioContextStarted) = React.useState(() => false);
    let (path, setPath) = React.useState(() => None);
    let (key, _setKey) = React.useState(() => Note.c4);
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
      let scale = Scale.make_of_string("C", Scale.major_intervals);
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
      // If the button_value has the same pitch as the guessNote, then play path
      // Js.log((
      // "local note is %d global note is %d\n",
      // Note.to_pos(local_note),
      // Note.to_pos(Option.get(guessNote)),
      // ));
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
    <div>
      <div className="button-container">
        <div className="note-grid">
          // TODO: Need the buttons to also resolve towards tonic
          // TODO: Need sidebar
          // TODO: Need function to make buttons (getting complicated to make changes to them all at once)
          // TODO: Global time value for speeding up/slowing down

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
    </div>;
    // TODO: Button for selecting scale notes
    // TODO: Something that learns how good you're getting at guessing and targets stuff you're bad at
    // TODO: Button for selecting scale root
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
