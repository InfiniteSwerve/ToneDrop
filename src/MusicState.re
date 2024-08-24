module Note = Music.Note;
module Chord = Music.Chord;
module Scale = Music.Scale;
module GuessableNotes = Music.GuessableNotes;
module Play = Synth.Play;

type highlight = Synth.highlight;

module InitialState = {
  type mode =
    | Play
    | PlayProgression
    | ChangeKey
    | ChangeScale
    | ChangeGuessableNotes;

  type musicState = {
    mode,
    synth: Synth.t,
    scale: Scale.t,
    path: list(Note.t),
    guessNote: Note.t,
    guessableNotes: GuessableNotes.t,
    bpm: int,
  };

  type t = musicState;

  let make = () => {
    let synth = Synth.createPolySynth();
    let scale = Scale.of_string("C", Scale.major_intervals);
    let guessableNotes = GuessableNotes.of_scale(scale);
    {
      mode: Play,
      synth,
      scale,
      path: [scale.root],
      guessNote: scale.root,
      guessableNotes,
      bpm: 80,
    };
  };
};

module type State = {
  let state: InitialState.t;
  let setState: (InitialState.t => InitialState.t) => unit;
  let noteHighlights: array(Synth.highlight);
  let setNoteHighlight:
    (array(Synth.highlight) => array(Synth.highlight)) => unit;
};

module State = (State: State) => {
  type t = InitialState.t;
  let s = State.state;
  let ss = State.setState;
  let noteHighlights = State.noteHighlights;
  let setNoteHighlight = State.setNoteHighlight;

  let changeMode = (mode: InitialState.mode) => {
    State.setState(s => {...s, mode});
  };

  let changeGuessableNotes = note => {
    ss(s =>
      {...s, guessableNotes: GuessableNotes.swap(s.guessableNotes, note)}
    );
  };

  let changePath = path => {
    ss(s => {...s, path});
  };

  let changeScale = scale => {
    ss(s => {...s, scale});
  };

  let stopAudio = () => {
    Synth.drop_audio(s.synth);
  };

  let playNote = note => {
    Play.note(s.synth, note);
  };

  let playChord = chord => {
    Play.chord(s.synth, chord);
  };

  let playPath = (path, highlight) => {
    Play.path(
      s.synth,
      State.setNoteHighlight,
      path,
      highlight,
      s.scale.root,
      s.bpm,
    );
  };

  let playNoteGetPath = () => {
    let guessNote = GuessableNotes.get_random_note(s.scale, s.guessableNotes);
    let path = Scale.get_path(s.scale, guessNote, s.scale.root);

    ss(s => {...s, guessNote, path});

    let two = Chord.(of_interval_kind(s.scale.root, 2, Minor));
    let five = Chord.(of_interval_kind(s.scale.root, 7, Major));
    let one = Chord.(of_interval_kind(s.scale.root, 0, Major));

    Play.chords_with_callback(
      s.synth,
      State.setNoteHighlight,
      s.scale.root,
      [two, five, one],
      s.bpm,
      () =>
      Play.note(s.synth, guessNote)
    );
    State.setNoteHighlight(_ => Array.make(13, Synth.Blank));
  };

  let playCadence = () => {
    let two = Chord.(of_interval_kind(s.scale.root, 2, Minor));
    let five = Chord.(of_interval_kind(s.scale.root, 7, Major));
    let one = Chord.(of_interval_kind(s.scale.root, 0, Major));
    Play.chords_with_callback(
      s.synth,
      State.setNoteHighlight,
      s.scale.root,
      [two, five, one],
      s.bpm,
      () =>
      playNote(s.guessNote)
    );
  };
};
