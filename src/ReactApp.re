module Note = Music.Note;
module Scale = Music.Scale;
module NoteGrid = {
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
    let className = 
      "note-button "
        ++ (accidental ? "sharp-flat ": "")

    let label = Printf.sprintf("%s", Note.to_solfege(Note.of_pos(noteValue)))
    let gridStyle =
      ReactDOM.Style.make(
        ~gridColumn=Printf.sprintf("%d / span 1", gridColumn),
        ~gridRow=Printf.sprintf("%d", gridRow),
        (),
      );

    <button
      key={Int.to_string(noteValue)}
      className={className}
    //  onClick={_event => handleNoteButtonClick(noteValue)}
      style=gridStyle>
      label->React.string
    </button>;

  };
  let make = () => {
    <div className="grid-container">
      <div className="note-grid">
        {Array.mapi(
           (noteValue, _) => makeNoteButton(~noteValue),
           Array.make(13,0),
         )
         |> React.array}
      </div>
    </div>
    
  };
}

module DropDown = {
  [@react.component]
  let make = (~show: bool) => {  // Added type annotation for clarity and labeled argument.
    if (show) {
      <div className="dropdown-content">
        <div> {"1"->React.string} </div>
        <div> {"2"->React.string} </div>
        <div> {"3"->React.string} </div>
      </div>
    } else {
      React.null;
    }
  };
};

module Button = {
  [@react.component]
  let make = (~id, ~label, ~onClick) => {
    <button className="button" id=id onClick> {React.string(label)} </button>;
  };
};

module Sidebar = {
  [@react.component]
  let make = () => {
    let (dropdownStates, setDropdownStates) = React.useState(() => Js.Dict.empty());

    let toggleDropdown = (id) => {
      setDropdownStates(prevStates => {
          Js.Dict.set(
          prevStates,
          id,
          Js.Dict.get(prevStates, id) -> Belt.Option.mapWithDefault(false, v => !v)
        );
        prevStates}
      );
    };

<div className="sidebar">
      <Button id="test1" label="Test 1" onClick={_event => toggleDropdown("test1")}/>
      {DropDown.make(~show=Js.Dict.get(dropdownStates, "test1")->Belt.Option.getWithDefault(false))}

      <Button id="test2" label="Test 2" onClick={_event => toggleDropdown("test2")}/>
      {DropDown.make(~show=Js.Dict.get(dropdownStates, "test2")->Belt.Option.getWithDefault(false))}
    </div>;
  };
};

module App = {
  [@react.component]
  let make = () => {
  <div className="app">
    {NoteGrid.make();}
    {Sidebar.make();}

  </div>
  };

}


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
