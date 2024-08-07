module Note = Music.Note;
module Scale = Music.Scale;
module Play = Synth.Play;

module MusicState = {
  /*
      probably want this to hold
     - current scale
     - current chord
     - current progression
     - synth
     - current path
   */
  type musicState = {
    scale: Scale.t,
    synth: Synth.t,
    setScale: (Scale.scale => Scale.scale) => unit,
  };
  type t = musicState;

  let make = () => {
    let (scale, setScale) =
      React.useState(() => Scale.of_string("C", Scale.major_intervals));
    let (synth, _setSynth) = React.useState(() => Synth.createPolySynth());
    {scale, synth, setScale};
  };

  let changeKey = (musicState: t, key: string, intervals: list(int)) => {
    musicState.setScale(_scale => {Scale.of_string(key, intervals)});
  };
};
module NoteGrid = {
  let handleNoteButtonClick = (musicState: MusicState.t, buttonValue) => {
    Synth.drop_audio(musicState.synth);
    let local_note = Note.transpose(musicState.scale.root, buttonValue);
    Play.note(musicState.synth, local_note);
  };

  let makeNoteButton = (musicState, noteValue) => {
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
    let className = "note-button " ++ (accidental ? "sharp-flat " : "");

    let label =
      Printf.sprintf("%s", Note.to_solfege(Note.of_pos(noteValue)));
    let gridStyle =
      ReactDOM.Style.make(
        ~gridColumn=Printf.sprintf("%d / span 1", gridColumn),
        ~gridRow=Printf.sprintf("%d", gridRow),
        (),
      );

    <button
      key={Int.to_string(noteValue)}
      className
      onClick={_event => handleNoteButtonClick(musicState, noteValue)}
      style=gridStyle>
      label->React.string
    </button>;
  };
  let make = musicState => {
    <div className="grid-container">
      <div className="note-grid">
        {Array.mapi(
           (noteValue, _) => makeNoteButton(musicState, noteValue),
           Array.make(13, 0),
         )
         |> React.array}
      </div>
    </div>;
  };
};

module DropDown = {
  [@react.component]
  let make = (~show: bool, ~onSelect: string => unit) => {
    show
      ? <div className="dropdown-content">
          <div className="dropdown-button" onClick={_ => onSelect("hi")}>
            {React.string("hi")}
          </div>
          <div className="dropdown-button" onClick={_ => onSelect("2")}>
            {React.string("2")}
          </div>
          <div className="dropdown-button" onClick={_ => onSelect("3")}>
            {React.string("3")}
          </div>
        </div>
      : React.null;
  };
};

module Button = {
  [@react.component]
  let make =
      (~id: string, ~label: string, ~onClick: React.Event.Mouse.t => unit) => {
    <button className="sidebar-button" id onClick>
      {React.string(label)}
    </button>;
  };
};

module Sidebar = {
  let make = (musicState: MusicState.t) => {
    let (_dropdownStates, setDropdownStates) =
      React.useState(() => Js.Dict.empty());
    let (_selections, setSelections) = React.useState(() => Js.Dict.empty());

    let toggleDropdown = id => {
      setDropdownStates(prevStates => {
        let newStates = Js.Dict.empty();
        Js.Dict.set(prevStates, id, true);
        Js.Dict.entries(prevStates)
        |> Array.iter(((key, value)) => Js.Dict.set(newStates, key, value));
        Js.Dict.set(
          newStates,
          id,
          !Belt.Option.getWithDefault(Js.Dict.get(prevStates, id), false),
        );
        Printf.printf("hi\n");
        newStates;
      });
    };

    let _handleSelect = (id, value) => {
      setSelections(prevSelections => {
        let newSelections = Js.Dict.empty();
        Js.Dict.entries(prevSelections)
        |> Array.iter(((key, value)) =>
             Js.Dict.set(newSelections, key, value)
           );
        Js.Dict.set(newSelections, id, value);
        newSelections;
      });
      toggleDropdown(id);
    };

    <div className="sidebar">
      <div className="dropdown">
        <Button
          id="key"
          label={Printf.sprintf("Key: %s", musicState.scale.key)}
          onClick={_ => toggleDropdown("key")}
        />
      </div>
    </div>;
  };
};
module App = {
  [@react.component]
  let make = () => {
    let (musicState, _setMusicState) =
      React.useState(() => MusicState.make());

    <div className="app">
      {NoteGrid.make(musicState)}
      {Sidebar.make(musicState)}
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
