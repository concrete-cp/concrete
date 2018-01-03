concrete
========

[![Build Status](https://travis-ci.org/concrete-cp/concrete.svg?branch=master)](https://travis-ci.org/concrete-cp/concrete)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/a5775c2b60e44d55865574114561d991)](https://www.codacy.com/app/scand1sk/concrete?utm_source=github.com&utm_medium=referral&utm_content=concrete-cp/concrete&utm_campaign=badger)
[![Codacy Badge](https://api.codacy.com/project/badge/Coverage/a1a0df7c57e24040a76ed7d5a7e609be)](https://www.codacy.com/app/scand1sk/concrete?utm_source=github.com&utm_medium=referral&utm_content=concrete-cp/concrete&utm_campaign=Badge_Coverage)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/fr.univ-valenciennes/concrete_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/fr.univ-valenciennes/concrete_2.12)

A CSP solving software & API

# Installation

Please find compiled software in the [releases](https://github.com/concrete-cp/concrete/releases/latest) section. 
Provided packages come with all required dependencies and scripts to run Concrete from the command line.

You can also find published releases on Maven Central for easy inclusion in your Java/Scala project:

    <dependency>
        <groupId>fr.univ-valenciennes</groupId>
        <artifactId>concrete_2.12</artifactId>
        <version>3.6</version>
    </dependency>

sbt dependency:

    libraryDependencies += "fr.univ-valenciennes" % "concrete" % "3.6"
    
# Running Concrete from command line

The easiest way to run concrete from command-line is to use scripts provided in the `bin/` directory.

To solve problems modelled using the [XCSP3](http://www.xcsp.org) language, use:

    bin/x-c-s-p3-concrete FILE.xml

The output of this script complies with the rules of the [XCSP3 2017 Competition](http://www.xcsp.org/competition).
    
To solve problems modelled using the [FlatZinc](http://www.minizinc.org) language, use:

    bin/f-z-concrete FILE.fzn
    
The output of this script complies with the rules of the [Minizinc Challenge 2017](http://www.minizinc.org/challenge.html).

If you want to solve a problem modelled using the MiniZinc format, 
please convert it to FlatZinc using the MiniZinc libraries provided
in the `mzn_lib/` directory.
    
Compressed formats can be used (e.g., `FILE.xml.xz` or `FILE.fzn.xz`).
Files will be inflated on-the-fly by the solver. 
Concrete uses [Apache Commons Compress](https://commons.apache.org/proper/commons-compress/)
to inflate compressed files and [XZ for Java](https://tukaani.org/xz/java.html) is included by default in the 
dependencies. Major file compression formats should be supported. 

## Command-line options

Concrete's command-line runner supports the following options, both for XCSP3 and FlatZinc formats:


    -a: output all solutions (for optimization problems, each new solution will be better than the previous one)
    -s: output statistics about the solving (such as the number of constraint propagations or decision nodes)
    -f: ignore solving heuristics defined in the problem files (only relevant for FlatZinc)
    -X*: these arguments will be given to the JVM (e.g., define max heap or stack size)


Many options are also available to tune Concrete's search strategies, 
but their number is too large to be listed here. For example, 
you can use `-variable.heuristic=DDegOnDom` to use the legacy
_dom/ddeg_ variable ordering heuristic instead of the default _dom/wdeg_. 
Please contact a developer if you need more information.

# Running Concrete from a running JVM

You can run Concrete from within a JVM application with a few lines of code. For example, in Scala:

    import concrete.runner._
    
    // Instantiate parameter manager
    val pm = new ParameterManager()
    
    val solution: Try[Map[String, Any]] = for {
        // Load the XCSP3 problem instance given an URL
        problem <- XCSP3Concrete.loadCSPOMURL(url)
        
        // Alternatively, load a FZ problem
        // problem <- FZConcrete.loadCSPOMURL(pm, url)
        
        // generate the solver
        solver <- Solver(pm, problem)
        if solver.hasNext()   
    } yield {
        solver.next()
    }
   
The `ParameterManager` can be used to hold various options used by Concrete.
For example, you can give the equivalent of the `-f` option from the command-line
and enforce the _dom/ddeg_ variable ordering heuristic with 

    val pm = new ParameterManager()
        .updated("f", ())
        .updated("variable.heuristic", classOf[DDegOnDom])
           
    
When the solver is generated, the original problem is transformed (compiled)
to (hopefully best) suit the solver's capabilities. Exceptions that may occur during
the compilation and solver generation are handled using Scala's `Try` API (hence the 
`for`/`yield` construct depicted in the example).
The obtained solver is an instance of `Iterator`. `hasNext/next` methods 
are used to iterate over all solutions (for a decision
problem) or to obtain the next best solution (for an optimization problem). If you
are only interested in the optimal solution of an optimization problem, simply
iterate until the last element is found (e.g., `solver.toIterable.lastOption`).
Remember that computing the optimal solution of a hard problem may be extremely
long (i.e., years of CPU time), so exploiting intermediate solutions should be helpful.
  
A solution maps variable names to values (may be boolean or integer). Beware that
integer 0 and 1 values may be returned as `false` and `true`, respectively. The
method `concrete.util.Math.any2int(value: Any): Int` can be used to enforce an
integer result.     
  
This simple example should be extended to manage exceptions or unsatisfiable
problems correctly.    

## Time limit

If you run Concrete as a standalone program and want to run it within a limited time, just send a SIGINT signal to Concrete's JVM
when you want it to stop. The GNU `timeout` utility is perfect to generate the signal once the
time limit is reached. When receiving the SIGINT signal, Concrete will try to terminate gracefully 
and will output best known solution, statistics and a TimeOut error message. In some situations, this 
procedure may be too long for you (e.g., if the JVM is running out of memory, or if
the propagation is very slow due to some problem characteristics or a bug). You can send a 
KILL message if you want to force the termination of Concrete process.

Enforcing a time limit on Concrete when ran from a Java application has not been tested.
Concrete can be ran a separate thread. Interrupting the thread should stop
the solving process and generate a `TimeoutException`.

# Features

Concrete is a CSP constraint solver written in Scala 2.12. We always try
to use up-to-date dependencies. Concrete is a pretty standard CP solver,
which solves CSP instances using depth-first search and variants of AC
for propagation. The two main specific aspects of Concrete 
are:
 
 * the use of [persistent data structures](http://www.cambridge.org/fr/academic/subjects/computer-science/programming-languages-and-applied-logic/purely-functional-data-structures#.WkwN5g-GREM.mailto)
for managing domain states and some constraint states. For many constraint states, semi-persistent
data structures (sparse sets)
 or backtrack-stable data (watched literals or residues) are preferred. 
 * the use of the 
companion project [CSPOM](http://github.com/concrete-cp/cspom), a
solver-independent modeling assistant able to perform automatic
reformulation such as constraint aggregation or common subexpression elimination.
CSPOM is able to parse problems written in [FlatZinc](http://www.minizinc.org),
[XCSP3](http://www.xcsp.org), the legacy XCSP2 format or its own Scala
DSL (yet to be documented).

Concrete supports models defined with signed 32-bit integers. Beware
that overflows may occur, especially for
quadratic constraints. Domain width must also be strictly less than 2³¹. 
Most overflows should raise exceptions, but
they also can lead to incorrect or missing solutions (please report bugs!). You should
 avoid using values exceeding ±2³⁰ (roughly ±10⁹). Excessively
  large domains may also raise memory issues unless they can
  be represented using a single interval thorough the search.
 Domains
are internally represented using either intervals, bit vectors or hash tries depending on the 
domain density. Set variables are currently not supported.

Concrete natively supports the following constraints:

- Extension (list of allowed or forbidden tuples). 
  An optimized algorithm should be automatically selected for
  binary constraints (AC3-bit+rm), positive tables or MDD.
  
- Linear (a·x + b·y + … {=/</≤/≠} k). 
  Bound consistency (except for ≠) or domain consistency for ternary constraints (using residues).

- Absolute value (x = |y|). Bound or domain consistency (using residues).

- Distance (x = |y - z|). Bound or domain consistency (using residues).

- All-different with 2-consistency or bound consistency.

- Cardinality (AtLeast/AtMost)

- Bin-packing 

- Channel (x(i) = j ↔ x(j) = i)

- Boolean clauses and XOR (using watched literals)

- Generalized nogoods (using watched literals and residues)

- Cumulative using profile and energetic reasoning

- Rectangle packing (diffN) using quad-trees and energetic reasoning

- Integer division and modulo. Bound or domain consistency (using residues)

- Element / Member (using watched literals and residues)

- Inverse (x(i) = j → y(j) = i) 

- Lex-Leq 

- Lex-Neq

- Min/Max

- Quadratic (x = y · z, x = y²). Bound or domain consistency (using residues).


All other documented MiniZinc constraints are supported via decomposition or reformulation.

All other XCSP3 constraints selected for the 2017 competition are supported via decomposition or reformulation.
Some XCSP3 constraints are not supported.

# Search strategies

Concrete solves CSP/COP using a depth-first tree search. When solving XCSP3 instances or if the `-f` option is
 enforced, the default variable ordering heuristic is _dom/wdeg_. 
The default value heuristic chooses the best known value first, then a random bound randomly. 
Search is restarted periodically to reduce long tails of search time.

Propagation queue is managed using a coarse-grained constraint-oriented propagation scheme
with dynamic and constraint-specific propagation ordering heuristic. Constraint
entailment is managed when it can be detected easily.

# License

Concrete is free software, relased under the terms of the [GNU LGPL 3.0](https://www.gnu.org/licenses/lgpl.txt) license.
Concrete is © Julien Vion, CNRS and Univ. Valenciennes.