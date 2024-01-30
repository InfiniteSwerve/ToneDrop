
            {makeButton(
               ~noteValue=1,
               ~label="Ra",
               ~handleClick=handleButtonPress,
               ~accidental=true,
             )}
            {makeButton(
               ~noteValue=3,
               ~label="Me",
               ~handleClick=handleButtonPress,
               ~accidental=true,
             )}
            <button
              className="note-button sharp-flat"
              onClick={_event => playTransposedNote(3)}
              style={ReactDOM.Style.make(~gridColumn="4 / span 2", ())}>
              "Me"->React.string
            </button>
            <button
              className="note-button sharp-flat"
              onClick={_event => playTransposedNote(6)}
              style={ReactDOM.Style.make(~gridColumn="8 / span 2", ())}>
              "Fi"->React.string
            </button>
            <button
              className="note-button sharp-flat"
              onClick={_event => playTransposedNote(8)}
              style={ReactDOM.Style.make(~gridColumn="10 / span 2", ())}>
              "Le"->React.string
            </button>
            <button
              className="note-button sharp-flat"
              onClick={_event => playTransposedNote(10)}
              style={ReactDOM.Style.make(~gridColumn="12 / span 2", ())}>
              "Te"->React.string
            </button>
          </div>
        <div className="note-grid">
          <button
            className="note-button"
            onClick={_event => playTransposedNote(0)}
            style={ReactDOM.Style.make(~gridColumn="1 / span 2", ())}>
            "Do"->React.string
          </button>
          <button
            className="note-button"
            onClick={_event => playTransposedNote(2)}
            style={ReactDOM.Style.make(~gridColumn="3 / span 2", ())}>
            "Re"->React.string
          </button>
          <button
            className="note-button"
            onClick={_event => playTransposedNote(4)}
            style={ReactDOM.Style.make(~gridColumn="5/ span 2", ())}>
            "Mi"->React.string
          </button>
          <button
            className="note-button"
            onClick={_event => playTransposedNote(5)}
            style={ReactDOM.Style.make(~gridColumn="7/ span 2", ())}>
            "Fa"->React.string
          </button>
          <button
            className="note-button"
            onClick={_event => playTransposedNote(7)}
            style={ReactDOM.Style.make(~gridColumn="9/ span 2", ())}>
            "So"->React.string
          </button>
          <button
            className="note-button"
            onClick={_event => playTransposedNote(9)}
            style={ReactDOM.Style.make(~gridColumn="11/ span 2", ())}>
            "La"->React.string
          </button>
          <button
            className="note-button"
            onClick={_event => playTransposedNote(11)}
            style={ReactDOM.Style.make(~gridColumn="13/ span 2", ())}>
            "Ti"->React.string
          </button>
          <button
            className="note-button"
            onClick={_event => playTransposedNote(12)}
            style={ReactDOM.Style.make(~gridColumn="15/ span 2", ())}>
            "Do"->React.string
          </button>
