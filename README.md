# undertone

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe-badge]][lfe]
[![Erlang Versions][erlang-badge]][versions]
[![Tag][github-tag-badge]][github-tag]

[![Project Logo][logo]][logo-large]

*Making Music with Extempore, OSC, and SuperCollider in LFE/OTP*

##### Table of Contents

* [About](#about-)
* [Status](#status-)
* [Presentations & Demos](#presentations--demos-)
* [Build and Test](#build-and-test-)
* [Usage Overview](#usage-overview-)
* [Documentation](#documentation-)
* [License](#license-)

## About [&#x219F;](#table-of-contents)

This is a project for making music in LFE, with support for MIDI, audio
processing, Open Sound Control, and more. The `undertone` project relies
heavily upon the phenomenal work of [Andrew Sorensen](https://github.com/digego)
et al in the [Extempore project](https://github.com/digego/extempore),
essentially having the aim of providing an LFE DSL for BEAM-native, distributed
interaction with Extempore.

SuperCollider support is currently limited to basic OSC operations. Features for
that or other backends or adding new backends to undertone will be prioritised
based upon time and interest.

Backend and OSC server versions tested against:

* Extempore 0.8.7
* SuperCollider 3.11.2
* Erlang OSC server 2.0 and 2.1
* Ardour 5 and 6

## Status [&#x219F;](#table-of-contents)

**Caution**: This is a work-in-progress under active development in the very
early stages of sound-systems integration in LFE: you have been warned! Loss of
limb or sanity are not the responsbility of the project nor the maintainers.

Current state of `undertone`:

* An OTP release-based project that automtically runs in the LFE REPL
* A fully functional Extempore REPL (any encountered issues are bugs --
  [file a ticket](https://github.com/lfex/undertone/issues/new)!) with an
  automatically reconnecting TCP client
* Basic Open Sound Control support (SuperCollider, the Ardour DAW)

For the current list of open tickets and the views by milestone, see the
following:

* https://github.com/lfex/undertone/issues
* https://github.com/lfex/undertone/milestones

## Presentations & Demos [&#x219F;](#table-of-contents)

https://undertone.lfe.io/presentations/


## Build and Test [&#x219F;](#table-of-contents)

```shell
$ rebar3 compile
$ rebar3 ltest # this step is optional
$ rebar3 release
```

## Usage Overview [&#x219F;](#table-of-contents)

For the examples below, start the REPL:

```shell
$ rebar3 repl
```
[![Banner Screenshot][banner-image]][banner-image]

Note that, while under active development, the undertone logging level will be
set to `debug`. If that's too much for you, before you start the REPL edit the
`./config/sys.config` file and change `level => debug` to `notice` or
`warning`.

Usage examples have been moved here, due to the rapid accretion of content:

* [A quick-start for Extempore in LFE](https://undertone.lfe.io/book/current/quick-start/extempore/index.html)
* [SuperCollider via OSC](https://undertone.lfe.io/book/current/quick-start/osc/index.html#supercollider-a-hreftable-of-contentsa)

## Documentation [&#x219F;](#table-of-contents)

There is an early-stage [documentation effort](https://github.com/cnbbooks/lfe-music-programming)
for this project, with draft content published at [undertone.lfe.io/book](https://undertone.lfe.io/book/).

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
[banner-image]: priv/images/banner-screenshot.png
