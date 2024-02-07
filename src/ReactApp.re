module Note = Music.Note;
module Chord = Music.Chord;
module Scale = Music.Scale;
module Play = Synth.Play;
module MusicState = Music.MusicState;
module GuessableNotes = Music.GuessableNotes;

type synthCallback = Synth.t => unit;

type state =
  | Play
  | ChangeKey
  | ChangeScale
  | ChangeGuessableNotes;

module App = {
  [@react.component]
  // Do as little shit as possible in this file. Do pretty much all the heavy lifting in music.ml.
  // TODO: Visualization of chord relative to key via p5.js
  // TODO: Do the functional ear trainer thing
  // TODO: ToneDrop logo in the top left
  // TODO: Global time value for speeding up/slowing down
  // TODO: Some way to save things
  // TODO: Something that learns how good you're getting at guessing and targets stuff you're bad at
  // TODO: Flash button during path playing
  // TODO: Prevent audio from playing again if it's already playing
  // TODO: Add progressions
  // TODO: Add progression editor
  let make = () => {
    let (state, setState) = React.useState(() => Play);
    let (synth, setSynth) = React.useState(() => None);
    let (_, setAudioContextStarted) = React.useState(() => false);
    let (path, setPath) = React.useState(() => None);
    let (scale, setScale) =
      React.useState(() => Scale.of_note(Note.c4, Scale.major_intervals));
    Js.log(Scale.to_string(scale));
    let (guessNote, setGuessNote) = React.useState(() => None);
    let (guessableNotes, setGuessableNotes) =
      React.useState(() =>
        [|
          true,
          false,
          true,
          false,
          true,
          true,
          false,
          true,
          false,
          true,
          false,
          true,
          true,
        |]
      );
    let (toggledButton, setToggledButton) = React.useState(() => None);
    let (scaleChangeRequested, setScaleChangeRequested) =
      React.useState(() => false);
    let (noteHighlight, setNoteHighlight) =
      React.useState(() => Array.make(13, `None));

    let handleSideBarButtonClick = (newState: state) => {
      state == newState ? setState(_ => Play) : setState(_ => newState);
    };
    let controlToggleButton = buttonId => {
      Some(buttonId) == toggledButton
        ? setToggledButton(_prevButtonId => None)
        : setToggledButton(_prevButtonId => Some(buttonId));
    };

    let sidebarButtonClass = (buttonState: state) =>
      state == buttonState
        ? "sidebar-button sidebar-button-toggled" : "sidebar-button";

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
      Js.log(("scale in play note", Scale.to_string(scale)));
      let note = GuessableNotes.get_random_note(scale, guessableNotes);
      let path = Scale.get_path(scale, note, scale.root);
      setGuessNote(_ => Some(note));
      setPath(_ => Some(path));

      let two = Chord.(of_interval_kind(scale.root, 2, Minor));
      let five = Chord.(of_interval_kind(scale.root, 7, Major));
      let one = Chord.(of_interval_kind(scale.root, 0, Major));
      Play.chords_with_callback(synth, scale.root, [two, five, one], 800, () =>
        Play.note(synth, note)
      );
      setNoteHighlight(_ => Array.make(13, `None));
      //ignore(Js.Global.setTimeout(() => {Play.note(synth, note)}, 800));
    };

    let playResolutionPath = synth => {
      switch (path) {
      | Some(actualPath) => Play.path(synth, actualPath, 300)
      | None => Js.log("Ain't no path to resolve")
      };
    };

    let noteButtonLabel = (noteValue: int) => {
      switch (state) {
      | ChangeKey =>
        Printf.sprintf(
          "%s",
          Note.notes[(noteValue + scale.root.pitch) mod 12],
        )
      | _ => Printf.sprintf("%s", Note.solfege[noteValue mod 12])
      };
    };

    let handleNoteButtonClick = button_value => {
      Printf.printf(
        "handleNoteButtonClick called with button_value: %d\n",
        button_value,
      );
      Printf.printf(
        "Size of noteHighlight array: %d\n",
        Array.length(noteHighlight),
      );
      switch (state) {
      | Play =>
        if (guessableNotes[button_value]) {
          let local_note = Note.transpose(scale.root, button_value);
          setNoteHighlight(prevHighlights => {
            Array.mapi(
              (index, _) =>
                if (index == button_value) {
                  if (Option.equal(Note.eq, Some(local_note), guessNote)) {
                    `Correct;
                  } else {
                    `Incorrect;
                  };
                } else {
                  prevHighlights[index];
                },
              prevHighlights,
            )
          });
          // Turn off highlighting after an instant
          ignore(
            Js.Global.setTimeout(
              () => {
                setNoteHighlight(prevHighlights =>
                  Array.mapi(
                    (index, _) =>
                      index == button_value ? `None : prevHighlights[index],
                    prevHighlights,
                  )
                )
              },
              200,
            ),
          );
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
        } else {
          ();
        }
      | ChangeGuessableNotes =>
        setGuessableNotes(notes => {
          GuessableNotes.swap(notes, button_value);
          notes;
        });
        Js.log(guessableNotes);
        controlToggleButton(Int.to_string(button_value));
      | ChangeScale =>
        setScale(oldScale => Scale.swap(oldScale, button_value));
        //Js.log(Scale.to_string(scale));
        controlToggleButton(Int.to_string(button_value));
      | _ => ()
      };
    };

    let isButtonDisabled = noteValue =>
      switch (state) {
      | ChangeScale => !Scale.mem(scale, noteValue)
      | _ => !guessableNotes[noteValue]
      };

    let makeNoteButton = (~noteValue) => {
      let accidental = List.mem(noteValue, [1, 3, 6, 8, 10]);
      let (gridRow, gridColumn) =
        switch (noteValue) {
        | 0 => (2, 1) /* C - Do */
        | 1 => (1, 1) /* C# - Ra */
        | 2 => (2, 2) /* D - Re */
        | 3 => (1, 2) /* D# - Me */
        | 4 => (2, 3) /* E - Mi */
        | 5 => (2, 4) /* F - Fa */
        | 6 => (1, 4) /* F# - Fi */
        | 7 => (2, 5) /* G - So */
        | 8 => (1, 5) /* G# - Le */
        | 9 => (2, 6) /* A - La */
        | 10 => (1, 6) /* A# - Te */
        | 11 => (2, 7) /* B - Ti */
        | _ => (2, 8) /* C - Do (octave) */
        };
      let disabled = isButtonDisabled(noteValue);
      let highlightClass =
        switch (noteHighlight[noteValue]) {
        | `None => ""
        | `Correct => "correct-note"
        | `Incorrect => "incorrect-note"
        };
      let className =
        "note-button "
        ++ (accidental ? "sharp-flat " : "")
        ++ (disabled ? "disabled " : "")
        ++ highlightClass;

      let stateClassName =
        switch (state) {
        | Play => "play-mode"
        | ChangeKey => "change-key-mode"
        | ChangeScale => "change-scale-mode"
        | ChangeGuessableNotes => "change-guessable-notes-mode"
        };

      let label = noteButtonLabel(noteValue);
      let gridStyle =
        ReactDOM.Style.make(
          ~gridColumn=Printf.sprintf("%d / span 1", gridColumn),
          ~gridRow=Printf.sprintf("%d", gridRow),
          (),
        );

      <button
        key={Int.to_string(noteValue)}
        className={className ++ " " ++ stateClassName}
        onClick={_event => handleNoteButtonClick(noteValue)}
        style=gridStyle>
        label->React.string
      </button>;
    };

    React.useEffect(() => {
      if (scaleChangeRequested) {
        withSynth(synth, playNoteGetPath);
        setScaleChangeRequested(_ => false);
      };

      None; // Return None for no cleanup, or Some(() => ...) for cleanup
    });

    <div className="app-container">
      <div className="sidebar">
        <button
          className={sidebarButtonClass(ChangeGuessableNotes)}
          onClick={_event => {handleSideBarButtonClick(ChangeGuessableNotes)}}>
          "Change Guessable Notes"->React.string
        </button>
        <button
          className={sidebarButtonClass(ChangeScale)}
          onClick={_event => {handleSideBarButtonClick(ChangeScale)}}>
          "Change Scale Notes"->React.string
        </button>
        <button
          className={sidebarButtonClass(ChangeKey)}
          onClick={_event => {handleSideBarButtonClick(ChangeKey)}}>
          {Printf.sprintf("Key: %s", scale.key)->React.string}
        </button>
        {switch (state) {
         | ChangeKey =>
           <div className="dropdown-content">
             <div
               onClick={_event =>
                 setScale(_ => Scale.of_string("C", scale.intervals))
               }>
               "C"->React.string
             </div>
             <div
               onClick={_event =>
                 setScale(_ => Scale.of_string("C#", scale.intervals))
               }>
               "C#/Db"->React.string
             </div>
             <div
               onClick={_event =>
                 setScale(_ => Scale.of_string("D", scale.intervals))
               }>
               "D"->React.string
             </div>
             <div
               onClick={_event =>
                 setScale(_ => Scale.of_string("D#", scale.intervals))
               }>
               "D#/Eb"->React.string
             </div>
             <div
               onClick={_event =>
                 setScale(_ => Scale.of_string("E", scale.intervals))
               }>
               "E"->React.string
             </div>
             <div
               onClick={_event =>
                 setScale(_ => Scale.of_string("F", scale.intervals))
               }>
               "F"->React.string
             </div>
             <div
               onClick={_event =>
                 setScale(_ => Scale.of_string("F#", scale.intervals))
               }>
               "F#/Gb"->React.string
             </div>
             <div
               onClick={_event =>
                 setScale(_ => Scale.of_string("G", scale.intervals))
               }>
               "G"->React.string
             </div>
             <div
               onClick={_event =>
                 setScale(_ => Scale.of_string("G#", scale.intervals))
               }>
               "G#/Ab"->React.string
             </div>
             <div
               onClick={_event =>
                 setScale(_ => Scale.of_string("A", scale.intervals))
               }>
               "A"->React.string
             </div>
             <div
               onClick={_event =>
                 setScale(_ => Scale.of_string("A#", scale.intervals))
               }>
               "A#/Bb"->React.string
             </div>
             <div
               onClick={_event =>
                 setScale(_ => Scale.of_string("B", scale.intervals))
               }>
               "B"->React.string
             </div>
           </div>
         | _ => React.null
         }}
      </div>
      <div className="main-content">
        <div className="button-container">
          <div className="note-grid">
            {Array.mapi(
               (noteValue, _) => makeNoteButton(~noteValue),
               guessableNotes,
             )
             |> React.array}
          </div>
          <div className="buttons-below-grid">
            <button
              className="function-button"
              id="repeat-question"
              onClick={_event => withSynth(synth, playNoteGetPath)}>
              "New Question"->React.string
            </button>
            <button
              className="function-button"
              id="repeat-note"
              onClick={_event =>
                // How to avoid option here?

                  withSynth(synth, _synth =>
                    Play.note(_synth, Option.get(guessNote))
                  )
                }>
              "Repeat Note"->React.string
            </button>
            <button
              className="function-button"
              id="play-answer"
              onClick={_event => withSynth(synth, playResolutionPath)}>
              "Play the Correct Answer"->React.string
            </button>
            <button
              className="function-button"
              id="new-question-new-key"
              onClick={_event => {
                setScaleChangeRequested(_ => true);
                setScale(_ => Scale.random_scale(scale));
              }}>
              //withSynth(synth, playNoteGetPath);
               "New Question Random Key"->React.string </button>
          </div>
        </div>
      </div>
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
