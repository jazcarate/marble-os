# marble-os
## Run things at your own pace
_A play on words with the phonetics of "marble os" and "marvelous"_

![Haskell CI](https://github.com/jazcarate/marble-os/workflows/Haskell%20CI/badge.svg)
![Release (latest SemVer)](https://img.shields.io/github/v/release/jazcarate/marble-os?sort=semver)
![License](https://img.shields.io/github/license/jazcarate/marble-os)

# Usage
1. Have a config file with the desired behavior
1. Run `marble` with the config file, piped to the program you want to run: `$ marble run ./sample/calculator.mbl | ./sample/calculator.sh`
1. ???
1. Profit

[![asciicast](https://asciinema.org/a/ffFLLTRD5ozZj0zqgDzS7rA7D.svg)](https://asciinema.org/a/ffFLLTRD5ozZj0zqgDzS7rA7D)

## Modes of running
A marble can be run isolated with the `run` command, or be synchronized with other marbles with the `sync` command.

## Run Options
| Option    | Description                                                        | Example         | Default value |
|-----------|--------------------------------------------------------------------|-----------------|---------------|
| delimiter | The character to delimit ticks                                     | --delimiter=x   | "-"           |
| split     | The character to split sequential outputs, with no tick in between | --split=x       | "|"           |
| lane      | If the file is multi-line, what line should it use                 | --lane=3        | 1             |
| inline    | Alternatively, you can provide an inline mbl format                | --inline="1--2" |               |

A normal MBL can have it's repeat strategy, name and tickrate inlined, but the CLI allows to override this settings with:
| Option | Description                                                                                               | Example    | Default value |
|--------|-----------------------------------------------------------------------------------------------------------|------------|---------------|
| repeat | Whether to repeat the sequence one it finishes. Possible values: `no`, `loop` or number of times          | --repeat=3 | No repeat     |
| name   | Name of the lane. This is different to `--lane` which chooses a lane in a `.mbl`, this override that name | --name=5s  | -             |
| tick   | Duration of each tick. This value will be overridden by the `tick` in the `.mbl` file                     | --tick=5s  | 1 second      |


Note: Ticks can be written in seconds, or with `us`, `ms`, `s`, `m` (microseconds, milliseconds, seconds or minutes, respectively) or any combination of them.
E.g.: 
* `$ marble --tick=5s` - 5 seconds
* `$ marble --tick=15` - 15 seconds
* `$ marble --tick=3m10s` - 3 minutes and 10 seconds _(this follows ISO 8601 duration spec. You can not jump units, and no spaces allowed)_
* `$ marble --tick=1500` - 1500 seconds = 25 minutes

This also applies to the first row of a `mbl`:
```mbl
tick: 1m
1---2--3
```

## Distributed Run Options
`marble` can also be daemonized to run in-sync from multiple places.
To do so, start `marble` with `sync` rather than `run`. This will wait until the corresponding `marble` is launched (via `marble daemon start`).

In addition to the [run options](#Run-Options), these are the additional distributed options
| Option | Description             | Example            | Default value |
|--------|-------------------------|--------------------|---------------|
| port   | Port to run the daemon. | --port=1337        | 3000          |
| host   | Host to run the daemon. | --host=192.168.3.3 | localhost     |


## mbl syntax
The `mbl` interpreter will parse the delimiter configured 👆 into "wait" and everything else into the output.
If you need to output the characters in the delimiter, you can user another delimiter that does not crash, or escape the output string with `\`.

e.g.: You can output "hyphenated-word" with this `mbl`:
```mbl
hyphenated\-word
```

If a `\` needs to be printed, it too has to be escaped.
e.g.:You can output "\latex{is my passion}" with this `mbl`:
```mbl
\\latex{is my passion}
```

If you need to print two things in succession, a `|` can be used to split the prints with no wait in between.
```mbl
2|3
```
Will print `2`, and then immediately `3`.

### Citations
If you need `marble` to output multiline, or otherwise too big of a thing to inline in in the `.mbl` syntax, you can write citation-like references.
A citation can be used multiple times, in multiple lanes. If a citation is described multiple times, the last one (the one closes to the end of the document) is the one taken into account.

e.g.: If you wanted to output "one", but what to still keep marble alignment, you can have this valid `.mbl`
```mbl
--1--
-2--1

[1]: one
```

If a citation matches part of the thing to print, it will get replaced with an implicit wait, to keep the `.mbl` file aligned. 
e.g.: To make it obvious that the `3` will get printed at the same time in both lanes, it's recommended to use citations as follow
```mbl
12-3
2--3

[1]: one
[2]: two
[3]: three
```

**Recommendation** In each `.mbl` use either all the prints as citations, and rest assured that the column alignment is correct, or do every print as _inline_ and keep in mind the file is no longer aligned.
Having both citations and inline prints, even though will get processed the same way, every time; might be interpreted counterintuitively.

### Named
Lanes can also be named like so:
```mbl
foo: 1-2
bar: 2-3
```
Where you can `run` this marble as: `$ marble run --lane=1` or `$ marble run --lane=foo`

The first lane would wait for 2 ticks and print `one`, where the second lane would wait 1 tick, print `2`, wait two extra ticks and print `one`.

# What is this?
I came across "marble diagrams" while looking at [JavaRX](https://rxjs-dev.firebaseapp.com/guide/testing/marble-testing).
I got inspired by [`yes`](https://man7.org/linux/man-pages/man1/yes.1.html), and sometimes I needed to run things at specific times.

# Development
Look at the makefile.
```bash
$ make help
```

e.g.: `$ make run ARGS="./sample/calculator.mbl --tick=30"`

# Prod
Build with `$ make build` and then run the executable `result/bin/marble`

# Missing

* Logs for daemon

### Bugs
Starting a `sync` with no demon started fails
> <socket: 16>: does not exist (Connection refused)