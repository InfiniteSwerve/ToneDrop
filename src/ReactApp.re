module Note = Music.Note;
module Chord = Music.Chord;
module Scale = Music.Scale;
module Play = Synth.Play;
module MusicState = Music.MusicState;
module GuessableNotes = Music.GuessableNotes;

type synthCallback = Synth.t => unit;

type state =
  | Play
  | PlayProgression
  | ChangeKey
  | ChangeScale
  | ChangeGuessableNotes;

module Dropdown = {
  [@react.component]
  let make = (~items, ~isVisible) => {
    <div className={isVisible ? "dropdown-visible" : "dropdown-hidden"}>
      {items
       |> List.map(item => <div key=item> item->React.string </div>)
       |> Array.of_list
       |> React.array}
    </div>;
  };
};

module Box = {
  [@react.component]
  let make = (~id) => {
    let _ = id;
    let (dropdown1Visible, setDropdown1Visible) = React.useState(() => false);
    let (dropdown2Visible, setDropdown2Visible) = React.useState(() => false);

    let toggleDropdown1 = () => setDropdown1Visible(prev => !prev);
    let toggleDropdown2 = () => setDropdown2Visible(prev => !prev);

    let toyData1 = ["Option 1", "Option 2", "Option 3"];
    let toyData2 = ["Choice A", "Choice B", "Choice C"];

    <div className="progression-grid-item chord-info-box">
      <div className="box-section" onClick={_event => toggleDropdown1()}>
        "Section 1"->React.string
        <Dropdown items=toyData1 isVisible=dropdown1Visible />
      </div>
      <div className="box-section" onClick={_event => toggleDropdown2()}>
        "Section 2"->React.string
        <Dropdown items=toyData2 isVisible=dropdown2Visible />
      </div>
    </div>;
  };
};

module ProgressionSection = {
  [@react.component]
  let make = () => {
    let (boxCount, setBoxCount) = React.useState(() => 11);

    let addBox = () => {
      setBoxCount(prevCount => prevCount + 1);
    };
    <div className="progression-container">
      <div className="progression-grid-container">
        {Array.init(boxCount, index =>
           <Box key={string_of_int(index)} id=index />
         )
         |> React.array}
        <div
          className="progression-grid-item add-chord-box"
          onClick={_event => addBox()}>
          "+"->React.string
        </div>
      </div>
    </div>;
  };
};

module App = {
  [@react.component]
  // Do as little shit as possible in this file. Do pretty much all the heavy lifting in music.ml.
  // NOTE: It feels like I'm having to add lots of extra code to just handle poor earlier decisions that I made, and like there should be ways to refactor/split up this code that improves the quality, the managability, is more idiomatic and shorter. I'm just not sure what exactly.
  // some ideas:
  //    - Break app up into multiple react components
  //    - Create a single interface for the audio/music to mess with. Call things from only one place and have an API call for each thing
  // TODO: Visualization of chord relative to key via p5.js
  // TODO: ToneDrop logo in the top left
  // TODO: Some way to save things
  // TODO: Something that learns how good you're getting at guessing and targets stuff you're bad at
  // TODO: Add progressions
  // TODO: Add progression editor
  // TODO: Maybe we should wrap all music state into one package in music.ml with a single interface?
  // BUG: Audio doesn't cancel even with drop_audio. There's an echo that can ring out
  // TODO: Start with a guessed note and path and synth so we don't need to mess with options, just disable the other buttons until we click the new question button
  // TODO: Add disableable logging for easier on-demand debugging
  // TODO: separate sliders for cadence speed + note speed
  // TODO: Make a guide on how to use + basic rules
  // TODO: Save user stats?
  // TODO: Better sounding instruments
  // TODO: Get outside review
  // TODO: For fitting scales to chords, add ability to choose more angular scales or try to keep the note similar
  // TODO: How does FET handle chord voicing?
  // TODO: Sample random progression in key
  // TODO: build chords from scales
  let make = () => {
    Random.init(int_of_float(Js.Date.now()));
    let (state, setState) = React.useState(() => Play);
    let (synth, _setSynth) = React.useState(() => Synth.createPolySynth());
    let (_, _setAudioContextStarted) = React.useState(() => true);
    let (path, setPath) = React.useState(() => [Note.c4]);
    let (scale, setScale) =
      React.useState(() => Scale.of_note(Note.c4, Scale.major_intervals));
    Js.log(Scale.to_string(scale));
    let (guessNote, setGuessNote) = React.useState(() => Note.c4);
    let (guessableNotes, setGuessableNotes) =
      React.useState(() => GuessableNotes.of_scale(scale));
    let (toggledButton, setToggledButton) = React.useState(() => None);
    let (scaleChangeRequested, setScaleChangeRequested) =
      React.useState(() => false);
    let (noteHighlight, setNoteHighlight) =
      React.useState(() => Array.make(13, `None));
    let (globalBPM, setGlobalBPM) =
      React.useState(() => {
        Synth.changeBPM(80);
        Synth.startTransport();
        80;
      });
    let (progressionNote, setProgressionNote) = React.useState(() => Note.c4);
    let (progressionRoot, setProgressionRoot) = React.useState(() => Note.c4);

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

    let playCadence = synth => {
      let two = Chord.(of_interval_kind(scale.root, 2, Minor));
      let five = Chord.(of_interval_kind(scale.root, 7, Major));
      let one = Chord.(of_interval_kind(scale.root, 0, Major));
      Play.chords_with_callback(
        synth, setNoteHighlight, scale.root, [two, five, one], globalBPM, () =>
        Play.note(synth, guessNote)
      );
    };

    let playNoteGetPath = synth => {
      Js.log(("scale in play note", Scale.to_string(scale)));
      let note = GuessableNotes.get_random_note(scale, guessableNotes);
      let path = Scale.get_path(scale, note, scale.root);
      setGuessNote(_ => note);
      setPath(_ => path);

      let two = Chord.(of_interval_kind(scale.root, 2, Minor));
      let five = Chord.(of_interval_kind(scale.root, 7, Major));
      let one = Chord.(of_interval_kind(scale.root, 0, Major));
      Play.chords_with_callback(
        synth, setNoteHighlight, scale.root, [two, five, one], globalBPM, () =>
        Play.note(synth, note)
      );
      setNoteHighlight(_ => Array.make(13, `None));
    };

    let playResolutionPath = (highlight, synth) => {
      Play.path(
        synth,
        setNoteHighlight,
        path,
        highlight,
        scale.root,
        globalBPM,
      );
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
      | Play when guessableNotes[button_value] =>
        Synth.drop_audio(synth);
        let local_note = Note.transpose(scale.root, button_value);
        Printf.printf("Playing %s\n", Note.to_string(local_note));
        Js.log(Note.to_string(local_note));
        let highlight =
          Note.eq(local_note, guessNote) ? `Correct : `Incorrect;
        let path = Scale.get_path(scale, local_note, scale.root);
        Play.path(
          synth,
          setNoteHighlight,
          path,
          highlight,
          scale.root,
          globalBPM,
        );
      | PlayProgression when guessableNotes[button_value] =>
        Synth.drop_audio(synth);
        let local_note = Note.transpose(scale.root, button_value);
        Printf.printf("Playing %s\n", Note.to_string(local_note));
        Printf.printf("Playing playProgression\n");
        Js.log(Note.to_string(local_note));
        let highlight =
          Note.eq(local_note, guessNote) ? `Correct : `Incorrect;
        let root_path = Scale.get_path(scale, local_note, progressionRoot);
        let scale_path = Scale.get_path(scale, local_note, scale.root);
        Play.path(
          synth,
          setNoteHighlight,
          root_path @ scale_path,
          highlight,
          scale.root,
          globalBPM,
        );
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
        | PlayProgression => "play-progression-mode"
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

    let handleBPMChange = event => {
      let newValue =
        React.Event.Form.target(event)##value |> int_of_string_opt;
      switch (newValue) {
      | Some(v) when v >= 60 && v <= 300 =>
        setGlobalBPM(_ => {
          Synth.changeBPM(v);
          Synth.startTransport();
          v;
        })
      | _ => Synth.startTransport()
      };
    };
    let makeScaleChange = (name, intervals) => {
      <div
        onClick={_event => {
          setScale(_ => Scale.of_note(scale.root, intervals));
          setGuessableNotes(_ => GuessableNotes.of_scale(scale));
        }}>
        name->React.string
      </div>;
    };

    let makeKeyChange = (root, visual) => {
      <div
        onClick={_event => {
          setScaleChangeRequested(_ => true);
          setScale(_ => Scale.of_string(root, scale.intervals));
        }}>
        visual->React.string
      </div>;
    };

    React.useEffect(() => {
      if (scaleChangeRequested) {
        playNoteGetPath(synth);
        setScaleChangeRequested(_ => false);
      };

      None;
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
             {makeKeyChange("C", "C")}
             {makeKeyChange("C#", "C#/Db")}
             {makeKeyChange("D", "D")}
             {makeKeyChange("D#", "D#/Eb")}
             {makeKeyChange("E", "E")}
             {makeKeyChange("F", "F")}
             {makeKeyChange("F#", "F#/Gb")}
             {makeKeyChange("G", "G")}
             {makeKeyChange("G#", "G#/Ab")}
             {makeKeyChange("A", "A")}
             {makeKeyChange("A#", "A#/Bb")}
             {makeKeyChange("B", "B")}
           </div>
         | _ => React.null
         }}
        <button
          className={sidebarButtonClass(ChangeScale)}
          onClick={_event => {handleSideBarButtonClick(ChangeScale)}}>
          {Printf.sprintf("Change Scale")->React.string}
        </button>
        {switch (state) {
         | ChangeScale =>
           <div className="dropdown-content">
             {makeScaleChange("Major", Scale.major_intervals)}
             {makeScaleChange("Minor", Scale.minor_intervals)}
             {makeScaleChange("Chromatic", Scale.chromatic_intervals)}
             {makeScaleChange("Dorian", Scale.dorian_intervals)}
             {makeScaleChange("Phrygian", Scale.phrygian_intervals)}
             {makeScaleChange("Lydian", Scale.lydian_intervals)}
             {makeScaleChange("Mixolydian", Scale.mixolydian_intervals)}
             {makeScaleChange("Locrian", Scale.locrian_intervals)}
           </div>
         | _ => <div />
         }}
        <div className="bpm-control">
          <div className="bpm-label"> "Global BPM"->React.string </div>
          <input
            type_="text"
            className="sidebar-input bpm-input"
            value={string_of_int(globalBPM)}
            onChange=handleBPMChange
          />
          <input
            type_="range"
            className="sidebar-slider bpm-slider"
            min="60"
            max="300"
            value={string_of_int(globalBPM)}
            onChange=handleBPMChange
          />
        </div>
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
              onClick={_event => {
                playNoteGetPath(synth);
                setState(_ => Play);
              }}>
              "New Question"->React.string
            </button>
            <button
              className="function-button"
              id="repeat-question"
              onClick={_event => playCadence(synth)}>
              "Repeat Question"->React.string
            </button>
            <button
              className="function-button"
              id="repeat-note"
              onClick={_event => Play.note(synth, guessNote)}>
              "Repeat Note"->React.string
            </button>
            <button
              className="function-button"
              id="play-answer"
              onClick={_event => playResolutionPath(`Correct, synth)}>
              "Play the Correct Answer"->React.string
            </button>
            <button
              className="function-button"
              id="new-question-new-key"
              onClick={_event => {
                setScaleChangeRequested(_ => true);
                setScale(_ => Scale.random_scale(scale));
              }}>
              "New Question Random Key"->React.string
            </button>
            <button
              className="function-button"
              id="play-scale"
              onClick={_event => {
                Play.path(
                  synth,
                  setNoteHighlight,
                  scale.notes
                  @ [Note.transpose(scale.root, 12)]
                  @ List.rev(scale.notes),
                  `Correct,
                  scale.root,
                  globalBPM,
                )
              }}>
              "Play Scale"->React.string
            </button>
            <button
              className="function-button"
              id="play-chord-note"
              onClick={_event => {
                Synth.drop_audio(synth);
                setState(_ => PlayProgression);

                // TODO: Get chord from scale
                let duration = 0.75 /. float_of_int(globalBPM / 60);
                setProgressionNote(_ => Scale.random_note(scale));
                setProgressionRoot(_ => Scale.random_note(scale));
                setGuessNote(_ => progressionNote);
                let root_path =
                  Scale.get_path(scale, progressionNote, progressionRoot);
                let scale_path =
                  Scale.get_path(scale, progressionNote, scale.root);
                // TODO: This shouldn't be minor the whole time
                Play.chord(synth, Chord.of_kind(progressionRoot, Minor));
                setPath(_ => root_path @ scale_path);
                //Js.log(Scale.list_to_string(Note.to_string, scale_path));
                Js.log(List.length(scale_path));
                Synth.schedule(
                  () => Play.note(synth, progressionNote),
                  Printf.sprintf("+%f", duration),
                );

                Synth.startTransport();
              }}>
              "new progression question"->React.string
            </button>
            <button
              className="function-button"
              id="play-progression-resolution-mode"
              onClick={_event => {
                let _duration = 0.75 /. float_of_int(globalBPM / 60);
                Synth.schedule(
                  () =>
                    Play.path(
                      synth,
                      setNoteHighlight,
                      path,
                      `Correct,
                      scale.root,
                      globalBPM,
                    ),
                  Printf.sprintf("+%f", 0.),
                );
              }}>
              "Resolve Progression Note"->React.string
            </button>
          </div>
        </div>
        <ProgressionSection />
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
