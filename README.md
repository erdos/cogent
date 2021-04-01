# cogent

A small rule engine with Equality Saturation<sup>[1]</sup> in Clojure.

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
Reasoning in first order logic     | work in progress
Elementary algebra                 | work in progress
Calculus: symbolic differentiation | work in progress
Performance improvements           | work in progress
General purpose simplifier         | design phase
General purpose solver             | design phase


## Resources

1. [egg: Fast and extensible equality saturation](https://dl.acm.org/doi/10.1145/3434304)
2. [Efficiency of a Good But Not Linear Set Union Algorithm](https://dl.acm.org/doi/10.1145/321879.321884)


## License

Copyright © 2021 Janos Erdos

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
