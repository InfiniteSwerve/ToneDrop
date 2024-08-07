module Note = Music.Note;
module Chord = Music.Chord;
module Scale = Music.Scale;
module Play = Synth.Play;
module GuessableNotes = Music.GuessableNotes;
module Progression = Music.Progression;
module InitialState = MusicState.InitialState;

type synthCallback = Synth.t => unit;

type mode = InitialState.mode

module Dropdown = {
  [@react.component]
  let make = (~items, ~isVisible, ~onSelect, ~toggleDropdown) => {
    let handleClick = (item, toggleDropdown) => {
      onSelect(item);
      toggleDropdown();
    };

    <div className={isVisible ? "dropdown-visible" : "dropdown-hidden"}>
      <div className="dropdown-x" onClick={_event => {toggleDropdown()}} />
      {items
       |> List.map(item =>
            <div
              key=item onClick={_event => handleClick(item, toggleDropdown)}>
              item->React.string
            </div>
          )
       |> Array.of_list
       |> React.array}
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
  let make = () => {
    //Random.init(int_of_float(Js.Date.now()));
    let (musicState, setMusicState) = React.useState(() => InitialState.make());
    let (noteHighlights, setNoteHighlight) = React.useState(() => Array.make(13, Synth.Blank));
    module State =
      MusicState.State({
        let state = musicState;
        let setState = setMusicState;
        let noteHighlights = noteHighlights;
        let setNoteHighlight = setNoteHighlight;
      });
    let (toggledButton, setToggledButton) = React.useState(() => None);

    let handleSideBarButtonClick = (newMode: mode) => {
      musicState.mode == newMode ? State.changeMode(InitialState.Play) : State.changeMode(newMode) ;
    };
    let controlToggleButton = buttonId => {
      Some(buttonId) == toggledButton
        ? setToggledButton(_prevButtonId => None)
        : setToggledButton(_prevButtonId => Some(buttonId));
    };

    let sidebarButtonClass = (buttonMode : mode) =>
      musicState.mode == buttonMode
        ? "sidebar-button sidebar-button-toggled" : "sidebar-button";

    let playCadence = () => {
      let two = Chord.(of_interval_kind(musicState.scale.root, 2, Minor));
      let five = Chord.(of_interval_kind(musicState.scale.root, 7, Major));
      let one = Chord.(of_interval_kind(musicState.scale.root, 0, Major));
      Play.chords_with_callback(
        musicState.synth, State.setNoteHighlight, musicState.scale.root, [two, five, one], musicState.bpm, () =>
        State.playNote(musicState.guessNote)
      );
    };

    let playResolutionPath = () => {
      State.playPath(musicState.path, Correct)
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
          Note.eq(local_note, musicState.guessNote) ? Synth.Correct : Synth.Incorrect;
        let path = Scale.get_path(musicState.scale, local_note, musicState.scale.root);
        List.iter(n => {Printf.printf("%s\n%!",Note.to_string(n))}, path);
        State.playPath(path, highlight)
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
      | _ => {
      !musicState.guessableNotes[noteValue]}
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
          {...s, bpm:v};
        })
      | _ => Synth.startTransport()
      };
    };
    let makeScaleChange = (name, intervals) => {
      <div
        onClick={_event => {
          State.changeScale(Scale.of_note(musicState.scale.root, intervals))
          setMusicState(s => {...s, guessableNotes: GuessableNotes.of_scale(musicState.scale)});
        }}>
        name->React.string
      </div>;
    };

    let makeKeyChange = (root, visual) => {
      <div
        onClick={_event => {
          setMusicState(s => {...s, scale: Scale.of_string(root, musicState.scale.intervals)});
        }}>
        visual->React.string
      </div>;
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
        <button
          className={sidebarButtonClass(ChangeKey)}
          onClick={_event => {handleSideBarButtonClick(ChangeKey)}}>
        </button>
        {switch (musicState.mode) {
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
        {switch (musicState.mode) {
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
        <div className="button-container">
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
            <div className="main-content-area">
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
                  onClick={_event => playCadence()}>
                  "Repeat Question"->React.string
                </button>
                <button
                  className="function-button"
                  id="repeat-note"
                  onClick={_event => Play.note(musicState.synth, musicState.guessNote)}>
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
                      @ List.rev(musicState.scale.notes), Correct)
                  }}>
                  "Play Scale"->React.string
                </button>
              </div>
            </div>
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
