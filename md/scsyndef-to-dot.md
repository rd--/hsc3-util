# scsyndef-to-dot

A _disassembler_ for UGen graphs, it reads the binary representation
of a [SuperCollider](http://audiosynth.com) _instrument_ and runs
[hsc3-dot](?t=hsc3-dot) on the unit-generator graph.

The _dot_ graph of the binary encoding of the [why
supercollider?](?t=hsc3-graphs&e=gr/why-supercollider.scd) graph,
[why_supercollider.scsyndef](sw/hsc3-graphs/scsyndef/why_supercollider.scsyndef),
is:

![](sw/hsc3-graphs/svg/why-supercollider.svg)

# Rationale

These types of drawings a `UGen` graphs are not dependent on the
language used to generate the graph, they do not record in any direct
way the processes that contructed the graph.

By disassembling the binary encoding of a graph, the [hsc3](?t=hsc3)
graph drawing codes can be used in other supercollider synthesiser
contexts.

Immediately, it makes maintenance of [rsc3-dot](?t=rsc3-dot) and
[sc3-dot](?t=sc3-dot) strictly unnecessary.
