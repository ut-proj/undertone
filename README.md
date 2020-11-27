# undertone

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe-badge]][lfe]
[![Erlang Versions][erlang-badge]][versions]
[![Tag][github-tag-badge]][github-tag]

[![Project Logo][logo]][logo-large]

*SuperCollider and OSC support in LFE*

##### Table of Contents

* [About](#about-)
* [Build](#build-)
* [Start the Project REPL](#start-the-repl-)
* [Tests](#tests-)
* [Usage](#usage-)
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

### Connecting to the Erlang OSC Server

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

