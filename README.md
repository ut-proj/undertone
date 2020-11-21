# undertone

[![Build Status][travis badge]][travis]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][versions]
[![Tag][github tag badge]][github tag]
[![Downloads][hex downloads]][hex package]

[![Project Logo][logo]][logo-large]

*SuperCollider support in LFE*

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
$ rebar3 lfe compile
```

# Start the Project REPL [&#x219F;](#table-of-contents)

```shell
$ rebar3 lfe repl
```

# Tests [&#x219F;](#table-of-contents)

```shell
$ rebar3 lfe test
```

## Usage [&#x219F;](#table-of-contents)

Start the REPL, per the above:

``` lisp
lfe> (application:start 'undertone)
ok
lfe> (undertone.server:echo "this is a test")
"this is a test"
```

## License [&#x219F;](#table-of-contents)

Apache License, Version 2.0

Copyright © 2020, Duncan McGreggor <oubiwann@gmail.com>.

<!-- Named page links below: /-->

[logo]: https://avatars1.githubusercontent.com/u/3434967?s=250
[logo-large]: https://avatars1.githubusercontent.com/u/3434967
[github]: https://github.com/ORG/undertone
[gitlab]: https://gitlab.com/ORG/undertone
[travis]: https://travis-ci.org/ORG/undertone
[travis badge]: https://img.shields.io/travis/ORG/undertone.svg
[gh-actions-badge]: https://github.com/ORG/undertone/workflows/Go/badge.svg
[gh-actions]: https://github.com/ORG/undertone/actions
[lfe]: https://github.com/rvirding/lfe
[lfe badge]: https://img.shields.io/badge/lfe-1.3.0-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-19%20to%2023-blue.svg
[versions]: https://github.com/ORG/undertone/blob/master/.travis.yml
[github tags]: https://github.com/ORG/undertone/tags
[github tags badge]: https://img.shields.io/github/tag/ORG/undertone.svg
[github downloads]: https://img.shields.io/github/downloads/ORG/undertone/total.svg
[hex badge]: https://img.shields.io/hexpm/v/undertone.svg?maxAge=2592000
[hex package]: https://hex.pm/packages/undertone
[hex downloads]: https://img.shields.io/hexpm/dt/undertone.svg
