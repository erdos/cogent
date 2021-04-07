# cogent

A small and naive rule engine with Equality Saturation<sup>[1]</sup> in Clojure.

![example workflow](https://github.com/erdos/cogent/actions/workflows/clojure.yml/badge.svg)
[![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/erdos/cogent/issues)
[![EPL 2.0](https://img.shields.io/badge/License-EPL%202.0-red.svg)](https://www.eclipse.org/legal/epl-2.0/)


## Usage

1. Install [Leiningen](https://leiningen.org/)
2. Clone this repository
2. Enter the REPL: `$ lein repl`

Check if two expressions are in the same e-class:

```Clojure
cogent.core=> (congruent? 3 '(d x (* 3 x)))
true

congruent.core=> (tautology? '(or x (and true (not x))))
true
```

## Status

Goal | Status
---- | ------
Equality Saturation engine         | done
Congruence checker                 | done
Tautology checker                  | done
Contradiction checker              | done
Performance improvements           | work in progress
General purpose simplifier         | design phase
General purpose solver             | design phase

Implemented rule sets:

- Calculus: symbolic differentiation: work in progress
- First order logic: work in progress
- Elementary algebra: work in progress
- SKI-calculus: done

## Resources

1. [egg: Fast and extensible equality saturation](https://dl.acm.org/doi/10.1145/3434304)
2. [Efficiency of a Good But Not Linear Set Union Algorithm](https://dl.acm.org/doi/10.1145/321879.321884)


## License

Copyright © 2021 Janos Erdos

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0. By using this software in any
fashion, you are agreeing to be bound by the terms of this license. You must not
remove this notice, or any other, from this software.
