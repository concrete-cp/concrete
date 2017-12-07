concrete
========

[![Build Status](https://travis-ci.org/concrete-cp/concrete.svg?branch=master)](https://travis-ci.org/concrete-cp/concrete)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/a5775c2b60e44d55865574114561d991)](https://www.codacy.com/app/scand1sk/concrete?utm_source=github.com&utm_medium=referral&utm_content=concrete-cp/concrete&utm_campaign=badger)
[![Codacy Badge](https://api.codacy.com/project/badge/Coverage/a1a0df7c57e24040a76ed7d5a7e609be)](https://www.codacy.com/app/scand1sk/concrete?utm_source=github.com&utm_medium=referral&utm_content=concrete-cp/concrete&utm_campaign=Badge_Coverage)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/fr.univ-valenciennes/concrete_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/fr.univ-valenciennes/concrete_2.12)

A CSP solving software & API

# Installation

Please find compiled software in the [releases] section. Provided packages come with all required dependencies.

You can also find published releases on Maven Central for easy inclusion in your Java/Scala project:

    <dependency>
        <groupId>fr.univ-valenciennes</groupId>
        <artifactId>concrete_2.12</artifactId>
        <version>3.6</version>
    </dependency>

sbt dependency:

    libraryDependencies += "fr.univ-valenciennes" % "concrete" % "3.6"
    
# Running Concrete

The easiest way to run concrete from command-line is to use scripts provided in the `bin/` directory.

To solve problems modelled using the [XCSP3](http://www.xcsp.org) language, use:

    bin/x-c-s-p3-concrete file.xml

The output of this script confers with the rules of the [XCSP3 2017 Competition](http://www.xcsp.org/competition).
    
To solve problems modelled using the [FlatZinc](http://www.minizinc.org) language, use:

    bin/f-z-concrete file.fzn
    
The output of this script confers with the rules of the [Minizinc Challenge 2017](http://www.minizinc.org/challenge.html).

If you want to solve a problem modelled using the MiniZinc format, 
please convert it to FlatZinc using the MiniZinc libraries provided
in the `mzn_lib/` directory.
    
Compressed formats can be used (e.g., `file.xml.xz`). Files will be deflated on-the-fly by the solver.

# Command-line options

Concrete supports the following options, both for XCSP3 and FlatZinc formats:

````
 -a: output all solutions (mostly relevant for optimization problems, as each new solution will be better than the previous one)
 -s: output statistics about the solving (such as the number of constraint propagations)
 -f: ignore solving heuristics defined in the problem files (FlatZinc only)
````

Many options are also available to tune Concrete's search strategies, but their number is too large to be listed here.
Please contact a developer if you need more information.

# Features

Concrete supports models defined with signed 32-bit integers. Domains
are internally represented using either intervals, bit vectors or hash tries depending on the 
domain density. Set variables are currently not supported.

Concrete natively supports the following constraints:

- Extension (list of allowed or forbidden tuples). 
  An optimized algorithm should be automatically selected for binary constraints, positive tables or MDD.
  
- Linear (a·x + b·y + … {=/</≤/≠} k). Bound consistency or domain consistency for ternary constraints.

- Absolute value (x = |y|). Bound or domain consistency.

- Distance (x = |y - z|). Bound or domain consistency.

- All-different. 2-consistency or bound consistency.

- AtLeast/AtMost

- Bin-packing 

- Channel (x(i) = j ↔ x(j) = i)

- Boolean clauses and XOR

- Cumulative 1D and 2D (DiffN) (profile and energetic reasoning)

- Integer division and modulo

- Element / Member

- Inverse (x(i) = j → y(j) = i) 

- Lex-Leq / Lex-Neq

- Min/Max

- Quadratic (x = y · z, x = y²). Bound or domain consistency.


All other MiniZinc constraints are supported via decomposition or reformulation.

All other XCSP3 constraints selected for the 2017 competition are supported via decomposition or reformulation.
Some XCSP3 constraints are not supported.

# Search strategies

Concrete solves CSP/COP using a depth-first tree search. When solving XCSP3 instances or if the `-f` option is
 enforced, the default variable ordering heuristic is dom/wdeg. 
The default value heuristic chooses the best known value first, then a random bound randomly. 
Search is restarted periodically.