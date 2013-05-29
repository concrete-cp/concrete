package javax.constraints.impl.search

import javax.constraints.SolutionIterator
import javax.constraints.Objective
import javax.constraints.Var
import concrete.generator.ProblemGenerator
import javax.constraints.ProblemState

class Solver(problem: javax.constraints.impl.Problem) extends javax.constraints.impl.search.AbstractSolver(problem) {
  val cspom = problem.cspom
  val concreteProblem = ProblemGenerator.generate(cspom)
  val concreteSolver = concrete.Solver(concreteProblem)

  // Members declared in 	 javax.constraints.impl.search.AbstractSolver
  def applySolution(x$1: javax.constraints.Solution): Boolean = ???

  def findSolution(ps: ProblemState): javax.constraints.Solution = {
    val s = concreteSolver.toIterable.headOption.map(new Solution(_)).getOrElse(null)
    if (ps == ProblemState.RESTORE) {
      concreteSolver.reset()
    }
    s
  }

  override def findAllSolutions: Array[javax.constraints.Solution] =
    (if (maxNumberOfSolutions < 0) {
      concreteSolver
    } else {
      concreteSolver.take(maxNumberOfSolutions)
    }) map {
      new Solution(_)
    } toArray

  override def solutionIterator: SolutionIterator = new SolutionIterator {
    val itr = concreteSolver
    def hasNext = itr.hasNext
    def next = new Solution(itr.next)
  }

  def newSearchStrategy(): javax.constraints.SearchStrategy = new SearchStrategy(this)

  override def findOptimalSolution(objective: Objective, variable: Var): Solution = {
    objective match {
      case Objective.MAXIMIZE => concreteSolver.maximize(concreteProblem.variable(variable.getName))
      case Objective.MINIMIZE => concreteSolver.minimize(concreteProblem.variable(variable.getName))
    }
    new Solution(concreteSolver.toIterable.last)
  }

  // Members declared in javax.constraints.Solver 
  def trace(x$1: javax.constraints.VarSet): Unit = ???
  def trace(x$1: Array[javax.constraints.Var]): Unit = ???
  def trace(x$1: javax.constraints.Var): Unit = ???

}