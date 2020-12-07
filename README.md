# undertone

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe-badge]][lfe]
[![Erlang Versions][erlang-badge]][versions]
[![Tag][github-tag-badge]][github-tag]

[![Project Logo][logo]][logo-large]

*Making Music with Extempore, OSC, and SuperCollider in LFE/OTP*

##### Table of Contents

* [About](#about-)
* [Build and Test](#build-and-test-)
* [Usage Overview](#usage-overview-)
  * [Working with Extempore](#working-with-extempore-)
  * [Open Sound Control](#osc-)
     * [The Erlang Server](#the-erlang-server-)
     * [SuperCollider](#supercollider-)
     * [Ardour](#ardour-)
* [Documentation](#documentation-)
* [License](#license-)

## About [&#x219F;](#table-of-contents)

This is a project for making music in LFE, with support for MIDI, audio
processing, Open Sound Control, and more. The `undertone` project relies
heavily upon the phenomenal work of [Andrew Sorensen](https://github.com/digego)
et al in the [Extempore project](https://github.com/digego/extempore),
essentially having the aim of providing an LFE DSL for BEAM-native, distributed
interaction with Extempore.

## Build and Test [&#x219F;](#table-of-contents)

```shell
$ rebar3 compile
$ rebar3 as test check
```

## Usage Overview [&#x219F;](#table-of-contents)

For the examples below, start the REPL:

```shell
$ rebar3 repl
```

### Working with Extempore [&#x219F;](#table-of-contents)

Tested against `extempore` `0.8.7`.

Download the latest Extempore from the
[project releases](https://github.com/digego/extempore/releases)
page on Github.

Assuming you've started `extempore` and have the LFE REPL running, as above,
connect to the Extempore TCP server

``` lisp
(xt:connect)
```

Load the appropriate Extempore files into the server and the LFE macros into the
current REPL session:

``` lisp
(xt:sys-load "examples/sharedsystem/setup.xtm")
(include-lib "undertone/include/xtl-patterns.lfe")
```

Play an ascending scale using the second synthesizer that comes with Extempore:

``` lisp
(/> 'ascending-scale 4 0 (play 'syn2 '@1 80 'dur) (scale 4 8))
```

Then change the tempo:

``` lisp
(set-tempo! 72)
```

You can stop the synth in two ways: changing 'play' form to the 'stop' form
while keeping the remaining body the same (this is useful for live coding
scenarios) or by calling the 'stop' form using just the pattern name you
defined in the 'play' form:

``` lisp
(// 'ascending-scale 4 0 (play 'syn1 '@1 80 'dur) (scale 4 8))
(// 'ascending-scale)
```

### OSC [&#x219F;](#table-of-contents)

#### The Erlang Server [&#x219F;](#table-of-contents)

Tested against `erlsci/osc` `2.0` and `2.1`.

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

#### SuperCollider [&#x219F;](#table-of-contents)

Tested against SuperCollider `3.11.2`.

#### Connecting

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

#### Playing Sounds

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

#### Ardour [&#x219F;](#table-of-contents)

``` lisp
lfe> (set c (undertone.ardour.client:connect "localhost" 3819))
;#(client #Pid<0.281.0> #Pid<0.282.0>)
lfe> (undertone.ardour.client:strip-list c)
;(#(name "SynthMaster One")
; #(strip-number 1)
; #(type "MIDI track")
; #(inputs 0)
; #(outputs 2)
; #(muted? 0)
; #(soloed? 0)
; #(record-enabled? 0))
```

## Documentation [&#x219F;](#table-of-contents)

There is an early-stage [documentation effort](https://github.com/cnbbooks/lfe-music-programming)
for this project, with draft content published [here](https://cnbbooks.github.io/lfe-music-programming/current/).

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
