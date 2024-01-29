// ToneInterop.js
// Why does the synth need to be so quiet otherwise it clips like shit
import * as Tone from 'tone';

export function createPolySynth() {
  const synth = new Tone.PolySynth(Tone.Synth, {
    oscillator: {
      volume: -20,
    }
  }).toDestination();
  return synth;
};

export function triggerAttackRelease(synth, note, duration) {
  synth.triggerAttackRelease(note, duration)

}

export function triggerListAttackRelease(synth, notes, duration) {
  synth.triggerAttackRelease(notes, duration)
}

export function playNotes(notes, synth) {
  const noteStrings = notes.map(note => note.toString());
  synth.triggerAttackRelease(noteStrings, "8n");
};
