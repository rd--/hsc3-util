# hsc3-sensel-ll

Command that connects to a Sensel device using the sensel-api and writes contact data to [Osc](https://ccrma.stanford.edu/groups/osc/index.html) packets.

Packet data is of the form: _/c_setn v 10 w x y z o i j p x? y?_ where
`v`=ctl-ix `w`=gate, `x`=x-axis, `y`=y-axis, `z`=z-axiz,
`i`=orientation, `j`=x-radius, `k`=y-radius,
`p`=pitch, `x?`=p-x-axis-distance `y?`=p-y-axis-distance

Voice data is not necessarily packed.  Control indices start at k0
(given by `-k`) and increment a number of places (given by `-i`) for
each voice.  (ie. k0, k0+i, k0+i+i...)

~~~~
$ hsc3-sensel-ll -h
hsc3-sensel-ll
  -a CHAR aspect ratio (default=i valid=[i,x,y])
  -c STR  connection protocol (default=tcp valid=[tcp,udp])
  -d      print device information (default=false)
  -e      latch z, ie. initial z value is retained (default=false)
  -f      set ContactsMinForce (default=24 valid=[8,16,24...])
  -g STR  set grid data (csv format) file name (default=nil)
  -h      print help
  -i INT  set index increment for voice data (default=10)
  -k INT  set k0 (default=13000)
  -l      illuminate leds (default=true)
  -m INT  set number of monitored contacts (default=16)
  -n STR  set hostname (default=localhost)
  -o INT  set v0 (default=0)
  -p INT  set port number (default=57110)
  -r INT  set scan rate (default=125 max=detail:medium:250,detail:low:1000)
  -s INT  set number of sequential Tcp/Udp ports voices are distributed across (default=1)
  -t      set text output mode (default=false)
  -v      set voice assign mode (default=true)
  -w STR  write trace text to output file (default=nil)
  -x INT  set scan detail (default=medium) 0=high 1=medium 2=low
  -z NUM  set z divisor (default=2048.0)
$
~~~~

The `-s` option allows for control data to be distributed across
multiple processes (and hence processors).

The argument determines the number of sequential Tcp/Udp ports, starting
from the port indicated by `-p`, that control messages should be
distributed across.

Setting `-m` 16 and `-s` 2 will result in 8 voices being controlled at
each of the two processes.  If there are two scsynth processes
running at ports 57110 and 57111 with the same 8-voice patch, then
the composite patch will have 16 voices.

(Note, this requires using jackd2, which supports multiple processors,
and not jackd1, which does not.)

The `-a` option sets the aspect ratio, ie. the scaling for each axis.
The sensor is wider (230mm) than it is tall (130mm).
`i` (the default) scales each axis independently to be in (0,1).
`x` scales each axis by the sensor width, x is in (0,1) y is in (0,0.57)
`y` scales each axis by the sensor height, y is in (0,1) x is in (0,1.78)

The `-e` option latches the _z_ field, that is it remains set at the level recorded at the start of the contact.
This makes testing programs intended for keyboard input, with an envelope generator, simpler.

The `-g` option reads grid data from a [Csv](https://www.ietf.org/rfc/rfc4180.txt) file. The fixed fields are:

1. row index
2. column index
3. x coordinate
4. y coordinate
5. pitch (fractional midi note number)
6. cell width
7. cell height
8. cell text (written in quotation marks, maximum fifteen characters)
9. cell polygon degree (4=rectangle, 6=hexagon)

Following are (x,y) co-ordinates for the cell polygon.
