module Note = Music.Note;
module Chord = Music.Chord;
module Scale = Music.Scale;
module GuessableNotes = Music.GuessableNotes;
module Progression = Music.Progression;
module Play = Synth.Play;
module InitialState = MusicState.InitialState;

type synthCallback = Synth.t => unit;

type mode = MusicState.InitialState.mode;

module Dropdown = {
  type item('a) = {
    label: string,
    value: 'a,
  };

  [@react.component]
  let make = (~label, ~items: list(item('a)), ~onSelect) => {
    let (isOpen, setIsOpen) = React.useState(() => false);

    <div className="dropdown">
      <button
        className="sidebar-button" onClick={_ => setIsOpen(prev => !prev)}>
        {React.string(label)}
      </button>
      {isOpen
         ? <div className="dropdown-menu">
             {items
              |> List.map(item =>
                   <button
                     key={item.label}
                     className="dropdown-item"
                     onClick={_ => {
                       onSelect(item.value);
                       setIsOpen(_ => false);
                     }}>
                     {React.string(item.label)}
                   </button>
                 )
              |> Array.of_list
              |> React.array}
           </div>
         : React.null}
    </div>;
  };
};

module Box = {
  [@react.component]
  let make = (~id, ~state: (module MusicState.STATE)) => {
    module State = (val state);
    let rootChoices: list(Dropdown.item(Note.t)) =
      Note.notes
      |> Array.to_list
      |> List.map((s: string) =>
           Dropdown.{label: s, value: Note.of_name(s, 4)}
         );

    let kindChoices: list(Dropdown.item(Chord.kind)) =
      List.map(
        (kind: Chord.kind) =>
          Dropdown.{label: Chord.kind_to_string(kind), value: kind},
        Chord.kinds,
      );

    let progressionIndexStyling =
      State.s.progressionIndex == id ? " current-chord" : "";

    let removeProgressionChord = id => {
      let progression = Progression.remove(State.s.progression, id);
      State.changeProgression(progression);
    };

    let handleRootSelect = (note: Note.t) => {
      let chord = Progression.get(State.s.progression, id);
      let new_chord = Chord.of_kind(note, chord.kind);
      Progression.swap(State.s.progression, id, new_chord)
      |> State.changeProgression;
    };

    let handleKindSelect = (kind: Chord.kind) => {
      let chord = Progression.get(State.s.progression, id);
      let new_chord = Chord.of_kind(chord.root, kind);
      Progression.swap(State.s.progression, id, new_chord)
      |> State.changeProgression;
    };

    <div
      className={String.cat(
        "chord-info-box",
        progressionIndexStyling,
      )}>
      <div
        className="dropdown-x" onClick={_event => removeProgressionChord(id)}>
        "X"->React.string
      </div>
      <div className="chord-button-container">
          <Dropdown
            label={
              Progression.get(State.s.progression, id).root |> Note.to_string
            }
            items=rootChoices
            onSelect=handleRootSelect
          />
          <Dropdown
            label={
              Progression.get(State.s.progression, id).kind
              |> Chord.kind_to_string
            }
            items=kindChoices
            onSelect=handleKindSelect
          />
      </div>
    </div>;
  };
};
module ProgressionSection = {
  [@react.component]
  let make = (~state: (module MusicState.STATE)) => {
    module State = (val state);
    let addBox = () => {
      let progression =
        Progression.add(State.s.progression, Chord.of_kind(Note.c4, Major));
      State.changeProgression(progression);
    };

    <div className="progression-container">
      <div className="progression-grid-container">
        {Array.mapi(
           (index, _) => <Box id=index state />,
           State.s.progression,
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
  // TODO: Visualization of chord relative to key via p5.js
  // TODO: ToneDrop logo in the top left
  // TODO: Some way to save things
  // TODO: Something that learns how good you're getting at guessing and targets stuff you're bad at
  // TODO: Maybe we should wrap all music state into one package in music.ml with a single interface?
  // BUG: Audio doesn't cancel even with drop_audio. There's an echo that can ring out
  // TODO: Add disableable logging for easier on-demand debugging
  // TODO: separate sliders for cadence speed + note speed
  // TODO: Make a guide on how to use + basic rules
  // TODO: Save user stats?
  // TODO: Better sounding instruments
  // TODO: Get outside review
  // TODO: For fitting scales to chords, add ability to choose more angular scales or try to keep the note similar
  // TODO: How does FET handle chord voicing?
  // TODO: Sample random progression in key
  // TODO: "start from here" on the chord progressions
  // TODO: Psuedorandomness on the note selection so you don't get like 5 in a row
  // TODO: Custom scales for each chord
  // TODO: Make disabled notes light up when played
  [@react.component]
  let make = () => {
    //Random.init(int_of_float(Js.Date.now()));
    let (musicState, setMusicState) =
      React.useState(() => InitialState.make());
    let (noteHighlights, setNoteHighlight) =
      React.useState(() => Array.make(13, Synth.Blank));

    // State is the master reference globally defined. Any changes to state happen to this
    module State =
      MusicState.State({
        let state = musicState;
        let setState = setMusicState;
        let noteHighlights = noteHighlights;
        let setNoteHighlight = setNoteHighlight;
      });

    let (toggledButton, setToggledButton) = React.useState(() => None);

    let handleSideBarButtonClick = (newMode: mode) => {
      musicState.mode == newMode
        ? State.changeMode(InitialState.Play) : State.changeMode(newMode);
    };
    let controlToggleButton = buttonId => {
      Some(buttonId) == toggledButton
        ? setToggledButton(_prevButtonId => None)
        : setToggledButton(_prevButtonId => Some(buttonId));
    };

    let sidebarButtonClass = (buttonMode: mode) =>
      musicState.mode == buttonMode
        ? "sidebar-button sidebar-button-toggled" : "sidebar-button";

    let playResolutionPath = () => {
      State.playPath(musicState.path, Correct);
    };

    let noteButtonLabel = (noteValue: int) => {
      switch (musicState.mode) {
      | ChangeKey =>
        Printf.sprintf(
          "%s",
          Note.notes[(noteValue + musicState.scale.root.pitch) mod 12],
        )
      | _ => Printf.sprintf("%s", Note.solfege[noteValue mod 12])
      };
    };

    let handleNoteButtonClick = button_value => {
      switch (musicState.mode) {
      | Play when musicState.guessableNotes[button_value] =>
        State.stopAudio();
        let local_note = Note.transpose(musicState.scale.root, button_value);
        let highlight =
          Note.eq(local_note, musicState.guessNote)
            ? Synth.Correct : Synth.Incorrect;
        let path =
          Scale.get_path(musicState.scale, local_note, musicState.scale.root);
        List.iter(n => {Printf.printf("%s\n%!", Note.to_string(n))}, path);
        State.playPath(path, highlight);
      | PlayProgression when musicState.guessableNotes[button_value] =>
        State.stopAudio()
      | ChangeGuessableNotes =>
        State.changeGuessableNotes(button_value);
        controlToggleButton(Int.to_string(button_value));
      | ChangeScale =>
        State.changeScale(Scale.swap(musicState.scale, button_value));
        controlToggleButton(Int.to_string(button_value));
      | _ => ()
      };
    };

    let isButtonDisabled = noteValue =>
      switch (musicState.mode) {
      | ChangeScale => !Scale.mem(musicState.scale, noteValue)
      | _ => !musicState.guessableNotes[noteValue]
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
        switch (State.noteHighlights[noteValue]) {
        | Blank => ""
        | Correct => "correct-note"
        | Incorrect => "incorrect-note"
        };
      let className =
        "note-button "
        ++ (accidental ? "sharp-flat " : "")
        ++ (disabled ? "disabled " : "")
        ++ highlightClass;

      let stateClassName =
        switch (musicState.mode) {
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
        setMusicState(s => {
          Synth.changeBPM(v);
          Synth.startTransport();
          {...s, bpm: v};
        })
      | _ => Synth.startTransport()
      };
    };

    let scaleOptions: list(Dropdown.item(list(int))) = [
      {label: "Major", value: Scale.major_intervals},
      {label: "Minor", value: Scale.minor_intervals},
      {label: "Chromatic", value: Scale.chromatic_intervals},
      {label: "Dorian", value: Scale.dorian_intervals},
      {label: "Phrygian", value: Scale.phrygian_intervals},
      {label: "Lydian", value: Scale.lydian_intervals},
      {label: "Mixolydian", value: Scale.mixolydian_intervals},
      {label: "Locrian", value: Scale.locrian_intervals},
    ];

    let keyOptions: list(Dropdown.item(string)) = [
      {label: "C", value: "C"},
      {label: "C#/Db", value: "C#"},
      {label: "D", value: "D"},
      {label: "D#/Eb", value: "D#"},
      {label: "E", value: "E"},
      {label: "F", value: "F"},
      {label: "F#/Gb", value: "F#"},
      {label: "G", value: "G"},
      {label: "G#/Ab", value: "G#"},
      {label: "A", value: "A"},
      {label: "A#/Bb", value: "A#"},
      {label: "B", value: "B"},
    ];

    let handleScaleChange = intervals => {
      let newScale = Scale.of_note(musicState.scale.root, intervals);
      let newGuessableNotes = GuessableNotes.of_scale(newScale);
      setMusicState(s =>
        {...s, scale: newScale, guessableNotes: newGuessableNotes}
      );
      State.changeScale(newScale);
    };

    let handleKeyChange = root => {
      let newScale = Scale.of_string(root, musicState.scale.intervals);
      let newGuessableNotes = GuessableNotes.of_scale(newScale);
      setMusicState(s =>
        {...s, scale: newScale, guessableNotes: newGuessableNotes}
      );
    };
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
        <Dropdown
          label="Change Key"
          items=keyOptions
          onSelect=handleKeyChange
        />
        <Dropdown
          label="Change Scale"
          items=scaleOptions
          onSelect=handleScaleChange
        />
        <div className="bpm-control">
          <div className="bpm-label"> "Global BPM"->React.string </div>
          <input
            type_="text"
            className="sidebar-input bpm-input"
            value={string_of_int(musicState.bpm)}
            onChange=handleBPMChange
          />
          <input
            type_="range"
            className="sidebar-slider bpm-slider"
            min="60"
            max="300"
            value={string_of_int(musicState.bpm)}
            onChange=handleBPMChange
          />
        </div>
      </div>
      <div className="main-content">
        <div className="main-content-area">
          <div className="grid-container">
            <div className="note-grid">
              {Array.mapi(
                 (noteValue, _) => makeNoteButton(~noteValue),
                 musicState.guessableNotes,
               )
               |> React.array}
            </div>
          </div>
          <div className="buttons-below-grid">
            <button
              className="function-button"
              id="repeat-question"
              onClick={_event => {
                State.playNoteGetPath();
                State.changeMode(Play);
              }}>
              "New Question"->React.string
            </button>
            <button
              className="function-button"
              id="repeat-question"
              onClick={_event => State.playCadence()}>
              "Repeat Question"->React.string
            </button>
            <button
              className="function-button"
              id="repeat-note"
              onClick={_event => State.playNote(musicState.guessNote)}>
              "Repeat Note"->React.string
            </button>
            <button
              className="function-button"
              id="play-answer"
              onClick={_event => playResolutionPath()}>
              "Play the Correct Answer"->React.string
            </button>
            <button
              className="function-button"
              id="new-question-new-key"
              onClick={_event => {
                State.changeScale(Scale.random_scale(musicState.scale));
                State.playNoteGetPath();
                State.changeMode(Play);
              }}>
              "New Question Random Key"->React.string
            </button>
            <button
              className="function-button"
              id="play-scale"
              onClick={_event => {
                State.playPath(
                  musicState.scale.notes
                  @ [Note.transpose(musicState.scale.root, 12)]
                  @ List.rev(musicState.scale.notes),
                  Correct,
                )
              }}>
              "Play Scale"->React.string
            </button>
          </div>
          <ProgressionSection state=(module State) />
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
