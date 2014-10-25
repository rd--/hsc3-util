# scsyndef-to-fs

A _disassembler_ for UGen graphs, it reads the binary representation
of a [SuperCollider](http://audiosynth.com) _instrument_ and prints an
[hsc3-forth](?t=hsc3-forth) notation of the unit-generator graph.

In the case of simple and direct graphs this notation is rather good, consider:

~~~~
let o = lfSaw KR (mce2 8 7.23) 0 * 3 + 80
    f = lfSaw KR 0.4 0 * 24 + o
    s = sinOsc AR (midiCPS f) 0 * 0.04
in out 0 (combN s 0.2 0.2 4)
~~~~

which is printed as:

~~~~
0 0.4 0 LFSaw.kr 24 * 8 0 LFSaw.kr 3 * 80 + + MIDICPS 0 SinOsc.ar
0.04 * 0.2 0.2 4 CombN 0.4 0 LFSaw.kr 24 * 7.23 0 LFSaw.kr 3 * 80
+ + MIDICPS 0 SinOsc.ar 0.04 * 0.2 0.2 4 CombN Out
~~~~

# Rationale

Used to help translate JMcC’s SC2 examples from haskell to FORTH,
see [hsc3-graphs](?t=hsc3-graphs).

# Caveat

This should, but does not, print the `mce` instruction for primitives
that halt mce expansion, ie. `EnvGen` and so on.
