module Note = Music.Note;
module Chord = Music.Chord;
module Scale = Music.Scale;
module GuessableNotes = Music.GuessableNotes;
module Progression = Music.Progression;
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
    progression: Progression.t,
    progressionIndex: int,
  };

  type t = musicState;

  let make = () => {
    let synth = Synth.createPolySynth();
    let scale = Scale.of_string("C", Scale.major_intervals);
    let guessableNotes = GuessableNotes.of_scale(scale);
    let progression = Progression.make();
    {
      mode: Play,
      synth,
      scale,
      path: [scale.root],
      guessNote: scale.root,
      guessableNotes,
      bpm: 80,
      progression,
      progressionIndex: 0,
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

module type STATE = {
  type t = InitialState.musicState;
  let s: InitialState.musicState;
  let ss: (InitialState.musicState => InitialState.musicState) => unit;
  let noteHighlights: array(highlight);
  let setNoteHighlight: (array(highlight) => array(highlight)) => unit;
  let changeMode: InitialState.mode => unit;
  let changeGuessableNotes: int => unit;
  let changePath: list(Note.note) => unit;
  let changeScale: Scale.scale => unit;
  let changeProgression: Progression.progression => unit;
  let removeProgressionChord: int => unit;
  let stopAudio: unit => unit;
  let playNote: Note.note => unit;
  let playChord: Chord.chord => unit;
  let playChords: list(Chord.t) => unit;
  let playPath: (list(Note.note), highlight) => unit;
  let playNoteGetPath: unit => unit;
  let playCadence: unit => unit;
};
// Try to keep this interface clean
// Just realized we don't need a record type here..
module State = (State: State) : STATE => {
  type t = InitialState.t;
  let s = State.state;
  let ss = State.setState;
  let noteHighlights = State.noteHighlights;
  let setNoteHighlight = State.setNoteHighlight;

  let changeMode = (mode: InitialState.mode) => {
    ss(s => {...s, mode});
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

  let changeProgression = progression => {
    ss(s => {...s, progression});
  };

  let changeProgressionIndex = progressionIndex => {
    ss(s => {...s, progressionIndex});
  };

  let removeProgressionChord = id => {
    let progression = Progression.remove(s.progression, id);
    ss(s => {...s, progression});
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

  let playChords = (chords) => {
      Play.chords(s.synth, changeProgressionIndex, setNoteHighlight, chords, s.bpm)};

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
