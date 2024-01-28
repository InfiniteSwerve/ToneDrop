// ToneInterop.js
import * as Tone from 'tone';

export function createPolySynth() {
  const synth = new Tone.PolySynth().toDestination();
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
}
