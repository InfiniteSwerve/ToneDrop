module Note = Music.Note;
module Chord = Music.Chord;
module Scale = Music.Scale;
module GuessableNotes = Music.GuessableNotes;

module InitialState = {
  type mode =
    | Play
    | PlayProgression
    | ChangeKey
    | ChangeScale
    | ChangeGuessableNotes;

  type musicState = {
    mode: mode,
    synth: Synth.t,
    scale: Scale.t,
    path: option(list(int)),
    guessNote: option(Note.t),
    guessableNotes: GuessableNotes.t,
  };

  type t = musicState;

  let make = () => {
    let synth = Synth.createPolySynth();
    let scale = Scale.of_string("C", Scale.major_intervals);
    let guessableNotes = GuessableNotes.of_scale(scale);
    {mode: Play, synth, scale, path: None, guessNote: None, guessableNotes};
  };
};

module type State = {
  let state: InitialState.t;
  let setState: (InitialState.t => InitialState.t) => unit;
};

module State = (State: State) => {
  type t = InitialState.t

  let changeMode = (mode: InitialState.mode) => {
    State.setState(s => {...s, mode})
  };
    

};
