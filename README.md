# marble-os
## Run things at your own pace
_A play on words with the phonetics of "marble os" and "marvelous"_

# Usage
1. Have a config file with the desired behavior
1. Run `marble` with the config file, piped to the program you want to run: `$ marble ./sample/calculator.mbl | ./sample/calculator.sh`
1. ???
1. Profit

## Options
| Option    | Description                                     | Example       | Default value |
|-----------|-------------------------------------------------|---------------|---------------|
| repeat    | Whether to repeat the sequence one it finishes  | --repeat      | false         |
| tick      | Duration of each tick                           | --tick=5s     | 1 second      |
| delimiter | The character to delimit ticks                  | --delimiter=x | "-"           |

Note: Ticks can be written in seconds, or with `us`, `ms`, `s`, `m` (microseconds, milliseconds, seconds or minutes, respectively) or any combination of them.
E.g.: 
* `$ marble --tick=5s` - 5 seconds
* `$ marble --tick=15` - 15 seconds
* `$ marble --tick=3m10s` - 3 minutes and 10 seconds _(this follows ISO 8601 duration spec. You can not jump units, and no spaces allowed)_
* `$ marble --tick=1500` - 1500 seconds = 25 minutes

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