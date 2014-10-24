# hash-paren

The [haskell](http://haskell.org) `do` notation ([Haskell
98](http://www.haskell.org/onlinereport) §3.14) requires us to assign
a _name_ to every monadic value we use.

In cases where a name is used only once, it may be much clearer if we
could write the value directly in the place where it is required.

`hash-paren` is a pre-processor that extends `do` notation by
rewriting `#` parenthesised elements of a right hand side expression
as monadic bindings, following a _left to right_ rule.  ie.

~~~~
do ...
   c <- f #(a) #(b)
   ...
~~~~

is rewritten as:

~~~~
do ...
   _hp_0 <- a
   _hp_1 <- b
   c <- f _hp_0 _hp_1
   ...
~~~~

# Rationale

In [hsc3](?t=hsc3) non-deterministic `UGen`s require _identifiers_ to
introduce distinct (unshared) values.

There is a _monadic_ form for writing these unit generators that
produces the identifiers automatically, the names for these forms have
an `M` suffix.

`UGen` graphs can have many non-deterministic nodes, and in many cases
having to name the _result_ of the monadic constructor is no better
than having to derive an identifier in the first place.

Consider two notations for the same graph, the _plain_ form:

    mce2 (whiteNoise 'α' AR) (pinkNoise 'β' AR)

and the monadic form:

    do {n0 <- whiteNoiseM AR; n1 <- pinkNoiseM AR; return (mce2 n0 n1)}

The monadic form has the advantage that it can itself be an element in
further _compositions_, since it will introduce unique identifiers at
each use.

However the plain form is shorter and clearer to read (and `hsc3`
provides a `clone` mechanism to partially address the
_non-composition_ problem).

The `#()` notation allows the first case to be re-written:

~~~~
do {return (mce2 #(whiteNoiseM AR) #(pinkNoiseM AR))}
~~~~

To illustrate the notation is a slightly more expanded context,
consider the plain graph:

~~~~
pond_life =
    let f0 = 20 + rand 'α' 0 30
        f1 = fSinOsc KR f0 0 * rand 'β' 100 400 + linRand 'γ' 500 2500 0
        a = lfPulse KR (3 / rand 'δ' 1 9) 0 (rand 'ε' 0.2 0.5) * 0.04
    in pan2 (sinOsc AR f1 0 * a) (rand 'ζ' (-1) 1) 0.5
~~~~

the `do` form:

~~~~
pond_life = do
  n0 <- randM 0 30
  let f0 = 20 + n0
  n1 <- randM 100 400
  n2 <- linRandM 500 2500 0
  let f1 = fSinOsc KR f0 0 * n1 + n2
  n3 <- randM 1 9
  n4 <- randM 0.2 0.5
  let a = lfPulse KR (3 / n3) 0 n4 * 0.04
  n5 <- randM (-1) 1
  return (pan2 (sinOsc AR f1 0 * a) n5 0.5)
~~~~

and the `#()` form:

~~~~
pond_life = do
  let f0 = 20 + #(randM 0 30)
  let f1 = fSinOsc KR f0 0 * #(randM 100 400) + #(linRandM 500 2500 0)
  let a = lfPulse KR (3 / #(randM 1 9)) 0 #(randM 0.2 0.5) * 0.04
  return (pan2 (sinOsc AR f1 0 * a) #(randM (-1) 1) 0.5)
~~~~

The `#()` notation shows the basic structure of the graph more
clearly, and avoids the requirement of introducing the _placeholder_
names of the `do` form.
