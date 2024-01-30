// Generated by Melange

import * as Caml_array from "melange.js/caml_array.js";
import * as Caml_option from "melange.js/caml_option.js";
import * as Curry from "melange.js/curry.js";
import * as Stdlib from "melange/stdlib.js";
import * as Stdlib__Array from "melange/array.js";
import * as Stdlib__Int from "melange/int.js";
import * as Stdlib__List from "melange/list.js";
import * as Stdlib__Printf from "melange/printf.js";
import * as Stdlib__Random from "melange/random.js";

var notes = [
  "C",
  "C#",
  "D",
  "D#",
  "E",
  "F",
  "F#",
  "G",
  "G#",
  "A",
  "A#",
  "B"
];

function eq(l, r) {
  if (l.pitch === r.pitch) {
    return l.octave === r.octave;
  } else {
    return false;
  }
}

function to_number(note) {
  return note.pitch;
}

function of_number(pitch, octave) {
  if (pitch > 11) {
    return Stdlib.invalid_arg("Number out of range");
  } else {
    return {
            pitch: pitch,
            octave: octave,
            scale_position: undefined
          };
  }
}

function of_pos(position) {
  var octave = position / 12 | 0;
  var pitch = position % 12;
  return of_number(pitch, octave);
}

function to_pos(note) {
  return note.pitch + Math.imul(note.octave, 12) | 0;
}

function of_name(note, octave) {
  var pitch_o = Stdlib__Array.find_index((function (name) {
          return note === name;
        }), notes);
  if (pitch_o !== undefined) {
    return of_number(pitch_o, octave);
  }
  throw Stdlib.failwith(Curry._1(Stdlib__Printf.sprintf(/* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "Note.of_name should always be able to find a note, but couldn't find ",
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: /* End_of_format */0
                      }
                    },
                    _1: "Note.of_name should always be able to find a note, but couldn't find %s"
                  }), note));
}

function to_string(note) {
  return Curry._2(Stdlib__Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* Int */4,
                      _0: /* Int_d */0,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: /* End_of_format */0
                    }
                  },
                  _1: "%s%d"
                }), Caml_array.get(notes, note.pitch), note.octave);
}

function transpose(scale_positionOpt, param) {
  var scale_position = scale_positionOpt !== undefined ? Caml_option.valFromOption(scale_positionOpt) : undefined;
  var octave = param.octave;
  var old_pitch = param.pitch;
  return function (operator) {
    var pitch = (old_pitch + operator | 0) % 12;
    var octaveChange = (old_pitch + operator | 0) / 12 | 0;
    var octave$1 = octave + octaveChange | 0;
    return {
            pitch: pitch,
            octave: octave$1,
            scale_position: scale_position
          };
  };
}

function transpose_notes(root, notes) {
  return Stdlib__List.map((function (operator) {
                return transpose(undefined, root)(operator);
              }), notes);
}

function compare(n1, n2) {
  return Stdlib__Int.compare(Math.imul(n1.octave, 12) + n1.pitch | 0, Math.imul(n2.octave, 12) + n2.pitch | 0);
}

function dist(from, target) {
  return Math.imul(12, target.octave - from.octave | 0) + (target.pitch - from.pitch | 0) | 0;
}

var c4 = of_number(0, 4);

var cs4 = of_number(1, 4);

var d4 = of_number(2, 4);

var ds4 = of_number(3, 4);

var e4 = of_number(4, 4);

var f4 = of_number(5, 4);

var fs4 = of_number(6, 4);

var g4 = of_number(7, 4);

var gs4 = of_number(8, 4);

var a4 = of_number(9, 4);

var as4 = of_number(10, 4);

var b4 = of_number(11, 4);

var Note = {
  notes: notes,
  eq: eq,
  to_number: to_number,
  of_number: of_number,
  of_pos: of_pos,
  to_pos: to_pos,
  of_name: of_name,
  to_string: to_string,
  transpose: transpose,
  transpose_notes: transpose_notes,
  compare: compare,
  dist: dist,
  c4: c4,
  cs4: cs4,
  d4: d4,
  ds4: ds4,
  e4: e4,
  f4: f4,
  fs4: fs4,
  g4: g4,
  gs4: gs4,
  a4: a4,
  as4: as4,
  b4: b4
};

var major_notes = {
  hd: 0,
  tl: {
    hd: 4,
    tl: {
      hd: 7,
      tl: /* [] */0
    }
  }
};

var minor_notes = {
  hd: 0,
  tl: {
    hd: 3,
    tl: {
      hd: 7,
      tl: /* [] */0
    }
  }
};

var dominant7_notes = {
  hd: 0,
  tl: {
    hd: 4,
    tl: {
      hd: 7,
      tl: {
        hd: 10,
        tl: /* [] */0
      }
    }
  }
};

var major7_notes = {
  hd: 0,
  tl: {
    hd: 4,
    tl: {
      hd: 7,
      tl: {
        hd: 11,
        tl: /* [] */0
      }
    }
  }
};

var minor7_notes = {
  hd: 0,
  tl: {
    hd: 3,
    tl: {
      hd: 7,
      tl: {
        hd: 10,
        tl: /* [] */0
      }
    }
  }
};

function spell(root, chord) {
  switch (chord) {
    case /* Major */0 :
        return transpose_notes(root, major_notes);
    case /* Minor */1 :
        return transpose_notes(root, minor_notes);
    case /* Dominant7 */2 :
        return transpose_notes(root, dominant7_notes);
    case /* Major7 */3 :
        return transpose_notes(root, major7_notes);
    case /* Minor7 */4 :
        return transpose_notes(root, minor7_notes);
    
  }
}

var Chord = {
  major_notes: major_notes,
  minor_notes: minor_notes,
  dominant7_notes: dominant7_notes,
  major7_notes: major7_notes,
  minor7_notes: minor7_notes,
  spell: spell
};

var major_intervals = {
  hd: 0,
  tl: {
    hd: 2,
    tl: {
      hd: 4,
      tl: {
        hd: 5,
        tl: {
          hd: 7,
          tl: {
            hd: 9,
            tl: {
              hd: 11,
              tl: /* [] */0
            }
          }
        }
      }
    }
  }
};

var minor_intervals = {
  hd: 0,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 5,
        tl: {
          hd: 7,
          tl: {
            hd: 8,
            tl: {
              hd: 10,
              tl: /* [] */0
            }
          }
        }
      }
    }
  }
};

var chromatic_intervals = {
  hd: 0,
  tl: {
    hd: 1,
    tl: {
      hd: 2,
      tl: {
        hd: 3,
        tl: {
          hd: 4,
          tl: {
            hd: 5,
            tl: {
              hd: 6,
              tl: {
                hd: 7,
                tl: {
                  hd: 8,
                  tl: {
                    hd: 9,
                    tl: {
                      hd: 10,
                      tl: {
                        hd: 11,
                        tl: /* [] */0
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
};

function get_intervals(kind) {
  switch (kind) {
    case /* Major */0 :
        return major_intervals;
    case /* Minor */1 :
        return minor_intervals;
    case /* Chromatic */2 :
        return chromatic_intervals;
    
  }
}

function make_of_string(key, intervals) {
  var root = of_name(key, 4);
  var notes = Stdlib__List.map((function (interval) {
          return transpose(Caml_option.some(interval), root)(interval);
        }), intervals);
  return {
          key: key,
          root: root,
          notes: notes,
          intervals: intervals
        };
}

function make_of_int(octaveOpt, key, intervals) {
  var octave = octaveOpt !== undefined ? octaveOpt : 4;
  var root = of_number(key, octave);
  var notes$1 = Stdlib__List.map((function (interval) {
          return transpose(Caml_option.some(interval), root)(interval);
        }), intervals);
  return {
          key: Caml_array.get(notes, key),
          root: root,
          notes: notes$1,
          intervals: intervals
        };
}

function make_big_scale(scale, lo, ho) {
  var _octave = ho;
  var _notes = /* [] */0;
  while(true) {
    var notes = _notes;
    var octave = _octave;
    var curr_octave_scale = make_of_int(octave, scale.root.pitch, major_intervals);
    var new_notes = Stdlib__List.map((function (note) {
            return note.pitch + Math.imul(note.octave, 12) | 0;
          }), curr_octave_scale.notes);
    var match = octave === lo;
    if (match) {
      return Stdlib__List.concat({
                  hd: new_notes,
                  tl: notes
                });
    }
    _notes = {
      hd: new_notes,
      tl: notes
    };
    _octave = octave - 1 | 0;
    continue ;
  };
}

function get_path(scale, start_note, end_note) {
  var d = dist(start_note, end_note);
  if (to_pos(start_note) === to_pos(end_note)) {
    return {
            hd: start_note,
            tl: /* [] */0
          };
  }
  var match = d >= 7 ? [
      {
        pitch: end_note.pitch,
        octave: end_note.octave - 1 | 0,
        scale_position: end_note.scale_position
      },
      start_note,
      /* Down */1
    ] : (
      d <= -7 ? [
          start_note,
          {
            pitch: end_note.pitch,
            octave: end_note.octave + 1 | 0,
            scale_position: end_note.scale_position
          },
          /* Up */0
        ] : (
          d > 0 ? [
              start_note,
              end_note,
              /* Up */0
            ] : [
              end_note,
              start_note,
              /* Down */1
            ]
        )
    );
  var upper = match[1];
  var lower = match[0];
  var lower_pos = to_pos(lower);
  var upper_pos = to_pos(upper);
  var big_scale = make_big_scale(scale, lower.octave, upper.octave);
  var result = Stdlib.$at({
        hd: lower,
        tl: Stdlib__List.map(of_pos, Stdlib__List.filter((function (pos) {
                    if (lower_pos < pos) {
                      return pos < upper_pos;
                    } else {
                      return false;
                    }
                  }), big_scale))
      }, {
        hd: upper,
        tl: /* [] */0
      });
  if (match[2]) {
    return Stdlib__List.rev(result);
  } else {
    return result;
  }
}

function random_note(scale) {
  return Stdlib__List.nth(scale.notes, Stdlib__Random.$$int(Stdlib__List.length(scale.intervals)));
}

function get_note_and_path(scale) {
  var note = random_note(scale);
  return [
          note,
          get_path(scale, note, scale.root)
        ];
}

var Scale = {
  major_intervals: major_intervals,
  minor_intervals: minor_intervals,
  chromatic_intervals: chromatic_intervals,
  get_intervals: get_intervals,
  make_of_string: make_of_string,
  make_of_int: make_of_int,
  make_big_scale: make_big_scale,
  get_path: get_path,
  random_note: random_note,
  get_note_and_path: get_note_and_path
};

export {
  Note ,
  Chord ,
  Scale ,
}
/* c4 Not a pure module */
