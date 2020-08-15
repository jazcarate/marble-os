# marble-os
## Run things at your own pace
_A play on words with the phonetics of "marble os" and "marvellous"_

# Usage
1. Have a config file with the desired behaviour
1. Run `marble` with the config file, piped to the program you want to run: `$ marble ./sample/calculator.mbl | ./sample/calculator.sh`
1. ???
1. Profit

## Options
| Option    | Description                                     | Example       | Default value |
|-----------|-------------------------------------------------|---------------|---------------|
| repeat    | Once the sequence completes, it will start over | --repeat      | false         |
| tick      | duration of each tick                           | --tick=5s     | 1 second      |
| delimiter | the character to delimit ticks                  | --delimiter=x | "-"           |

# What is this?
I came across "marble diagrams" while looking at [JavaRX](https://rxjs-dev.firebaseapp.com/guide/testing/marble-testing).
I got inspired by [`yes`](https://man7.org/linux/man-pages/man1/yes.1.html), and sometimes I needed to run things at specific times.

# Development
Look at the makefile.
```bash
$ make help
```

# Prod
Build with `$ make build` and then run the executable `result/bin/marble`