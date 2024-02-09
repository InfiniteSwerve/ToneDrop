// ToneInterop.js
// Why does the synth need to be so quiet otherwise it clips like shit
import * as Tone from 'tone';

export function createPolySynth() {
  const synth = new Tone.PolySynth(Tone.Synth, {
    oscillator: {
      volume: -3,
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

export function schedule(event, time) {
  Tone.Transport.schedule(event, time)
}

export function startTransport() {
  Tone.Transport.start()
}

export function stopTransport() {
  Tone.Transport.stop()
}

// Do I need to stop before canceling?
// No
export function clearTransport() {
  Tone.Transport.cancel()
}

export function releaseAll(synth) {
  synth.releaseAll();
}
