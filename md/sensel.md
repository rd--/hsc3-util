# hsc3-sensel

Command that connects to a Sensel device using the sensel-api and writes contact data to OSC packets.

Packet data is of the form: /c_setn k 8 g x y z o rx ry p px py where
`k`=ctl-ix `g`=gate, `x`=x-axis, `y`=y-axis, `z`=z-axiz,
`o`=orientation, `rx`=x-radius, `ry`=y-radius,
`p`=pitch, `px`=p-x-axis-distance `py`=p-y-axis-distance

Voice data is not necessarily packed.  Control indices start at k0
(given by `-k`) and increment a number of places (given by `-i`) for
each voice.  (ie. k0, k0+i, k0+i+i...)

~~~~
$ hsc3-sensel -h
hsc3-sensel
  -d      print device information (default=false)
  -f      set ContactsMinForce (default=24 valid=[8,16,24...])
  -g STR  set grid data (csv format) file name (default=nil)
  -h      print help
  -i INT  set index increment for voice data (default=10)
  -k INT  set k0 (default=13000)
  -m INT  set number of monitored contacts (default=16)
  -n STR  set hostname (default=localhost)
  -p INT  set port number (default=57110)
  -r INT  set scan rate (default=125)
  -s INT  set number of sequential UDP ports voices are distributed across (default=1)
  -t      set text output mode (default=false)
  -v      set voice assign mode (default=false)
  -x      set scan detail to high (default=medium)
  -z NUM  set z divisor (default=2048.0)
$
~~~~

The `-s` option allows for control data to be distributed across
multiple processes (and hence processors).

The argument determines the number of sequential UDP ports, starting
from the port indicated by `-p`, that control messages should be
distributed across.

Setting `-m` 16 and `-s` 2 will result in 8 voices being controlled at
each of the two processes.  If there are two scsynth processes
running at ports 57110 and 57111 with the same 8-voice patch, then
the composite patch will have 16 voices.

(Note, this requires using jackd2, which supports multiple processors,
and not jackd1, which does not.)
