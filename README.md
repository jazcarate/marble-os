# marble-os
## Run things at your own pace
_A play on words with the phonetics of "marble os" and "marvelous"_

![Haskell CI](https://github.com/jazcarate/marble-os/workflows/Haskell%20CI/badge.svg)
![Release (latest SemVer)](https://img.shields.io/github/v/release/jazcarate/marble-os?sort=semver)
![License](https://img.shields.io/github/license/jazcarate/marble-os)

# Usage
1. Have a config file with the desired behavior
1. Run `marble` with the config file, piped to the program you want to run: `$ marble ./sample/calculator.mbl | ./sample/calculator.sh`
1. ???
1. Profit

[![asciicast](https://asciinema.org/a/ffFLLTRD5ozZj0zqgDzS7rA7D.svg)](https://asciinema.org/a/ffFLLTRD5ozZj0zqgDzS7rA7D)

## Options
| Option    | Description                                        | Example       | Default value |
|-----------|----------------------------------------------------|---------------|---------------|
| repeat    | Whether to repeat the sequence one it finishes     | --repeat      | false         |
| tick      | Duration of each tick                              | --tick=5s     | 1 second      |
| delimiter | The character to delimit ticks                     | --delimiter=x | "-"           |
| lane      | If the file is multi-line, what line should it use | --lane=3      | 1             |

Note: Ticks can be written in seconds, or with `us`, `ms`, `s`, `m` (microseconds, milliseconds, seconds or minutes, respectively) or any combination of them.
E.g.: 
* `$ marble --tick=5s` - 5 seconds
* `$ marble --tick=15` - 15 seconds
* `$ marble --tick=3m10s` - 3 minutes and 10 seconds _(this follows ISO 8601 duration spec. You can not jump units, and no spaces allowed)_
* `$ marble --tick=1500` - 1500 seconds = 25 minutes

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
### Citations
Extend .mbl to allow citation-like commands.
eg:

```
# foo.mbl
---1--2

[1]: Say something
```
This would, in 4 ticks, output `Say something`

### TUI for the daemon
`brics` tui to see all connected "sync" and .mbl before running.