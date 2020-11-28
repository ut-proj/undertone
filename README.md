# undertone

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe-badge]][lfe]
[![Erlang Versions][erlang-badge]][versions]
[![Tag][github-tag-badge]][github-tag]

[![Project Logo][logo]][logo-large]

*Experimenting with OSC, the SuperCollider synth server, and more in LFE*

##### Table of Contents

* [About](#about-)
* [Build](#build-)
* [Start the Project REPL](#start-the-repl-)
* [Tests](#tests-)
* [Usage](#usage-)
  * [Connecting to the Erlang OSC Server](#connecting-to-the-erlang-osc-server-)
  * [Connecting to SuperCollider](#connecting-to-supercollider-)
  * [Playing Sounds in SuperCollider](#-playing-sounds-in-supercollider)
* [License](#license-)

## About [&#x219F;](#table-of-contents)

TBD

## Build [&#x219F;](#table-of-contents)

```shell
$ rebar3 compile
```

## Start the Project REPL [&#x219F;](#table-of-contents)

```shell
$ rebar3 repl
```

## Tests [&#x219F;](#table-of-contents)

```shell
$ rebar3 as test check
```

## Usage [&#x219F;](#table-of-contents)

For the examples below, start the REPL per the above.

### Connecting to the Erlang OSC Server [&#x219F;](#table-of-contents)

``` lisp
lfe> (set c (undertone.osc.client:connect "localhost" 2357))
; ok#(client #Pid<0.276.0> #Pid<0.277.0>)
lfe> (undertone.osc.client:echo c)
; ok
```

If you view the output of the running Erlang OSC server, you should see some
debug output logged:

```
=INFO REPORT==== 27-Nov-2020::16:14:05.986773 ===
Received message: []
```

You can also check passed arguements:

``` lisp
lfe> (undertone.osc.client:echo c '(a list of args))
ok
```

```
=INFO REPORT==== 27-Nov-2020::16:35:28.652299 ===
Received message: [a,list,'of',args]
```

### Connecting to SuperCollider [&#x219F;](#table-of-contents)

Start up the SuperCollider GUI / IDE, then in the editor enter the following:

```
s.options.maxLogins = 8;
s.reboot;
Server.default.options.asOptionsString
```

The last line will help you confirm the current settings, showing what they
would be if you'd started `scsynth` manually. In particular, you want to confirm
that you see `-l 8`.

Then, in a terminal at the LFE REPL:

``` lisp
lfe> (set c (undertone.sc.client:connect "localhost" 57110))
;#(client #Pid<0.344.0> #Pid<0.345.0>)
```

Once the client is set up, do a quick check to see that you are connected to the
right server / version and that there are synthesizer definitions loaded:

``` lisp
lfe> (undertone.sc.client:version c)
;(#(version "3.11.2") #(branch "HEAD") #(commit-id "9a34118e"))

lfe> (undertone.sc.client:status c)
;(#(unit-generators 0)
; #(synths 0)
; #(gruops 9)
; #(loaded-synth-definitions 106)
; #(cpu-average-usage 0.030904095619916916)
; #(cpu-peak-usage 0.2695612609386444)
; #(nominal-sample-rate 4.41e4)
; #(actual-sample-rate 44099.98856935304))
```

### Playing Sounds in SuperCollider [&#x219F;](#table-of-contents)

First create a handful of instances of the default synth and then stop them,
until we're ready:

``` lisp
lfe> (set synth-ids '(1000 1001 1002 1003 1004))
;"ϨϩϪϫϬ"
lfe> (set (list s0 s1 s2 s3 s4) (list-comp
       ((<- id synth-ids))
       (undertone.sc.client:create-synth c id)))
;"ϨϩϪϫϬ"
lfe> (list-comp ((<- id synth-ids)) (undertone.sc.client:stop-node c id))
;(ok ok ok ok ok)
```

Instead of a boring C chord, let's take some notes from the C scale's Locrian
mode using `B`, `E`, `F`, `A`, and `B` (a combination _rarely_ heard!):

``` lisp
lfe> (include-lib "include/notes.lfe")
;|-- loaded include: notes --|
lfe> (undertone.sc.client:set-node c s0 `("freq" ,(B3) out 0))
;ok
lfe> (undertone.sc.client:set-node c s1 `("freq" ,(E3) out 1))
;ok
lfe> (undertone.sc.client:set-node c s2 `("freq" ,(F3) out 0))
;ok
lfe> (undertone.sc.client:set-node c s3 `("freq" ,(A3) out 0))
;ok
lfe> (undertone.sc.client:set-node c s4 `("freq" ,(B4) out 1))
;ok
```

For the MIDI-minded, there is also an include file with functions defined for
getting notes by MIDI number.

And then, when you're done listening to that beautiful dissonance:

``` lisp
lfe> (list-comp ((<- id synth-ids)) (undertone.sc.client:start-node c id))
;(ok ok ok ok ok)
```

## License [&#x219F;](#table-of-contents)

Apache License, Version 2.0

Copyright © 2020, Duncan McGreggor <oubiwann@gmail.com>.


[//]: ---Named-Links---

[logo]: priv/images/logo-v1.png
[logo-large]: priv/images/logo-v1-large.png
[github]: https://github.com/lfex/undertone
[gh-actions-badge]: https://github.com/lfex/undertone/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfex/undertone/actions
[lfe]: https://github.com/rvirding/lfe
[lfe-badge]: https://img.shields.io/badge/lfe-2.0-blue.svg
[erlang-badge]: https://img.shields.io/badge/erlang-19%20to%2023-blue.svg
[versions]: https://github.com/lfex/undertone/blob/master/.github/workflows/cicd.yml
[github-tag]: https://github.com/lfex/undertone/tags
[github-tag-badge]: https://img.shields.io/github/tag/lfex/undertone.svg
[github-downloads]: https://img.shields.io/github/downloads/lfex/undertone/total.svg
