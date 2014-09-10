package javax.constraints.impl.search

import javax.constraints.SolutionIterator
import javax.constraints.Objective
import javax.constraints.Var
import concrete.generator.ProblemGenerator
import javax.constraints.ProblemState
import concrete.ParameterManager
import concrete.MAC
import concrete.Variable

class Solver(problem: javax.constraints.impl.Problem, params: ParameterManager) extends javax.constraints.impl.search.AbstractSolver(problem) {
  val cspom = problem.cspom
  val concreteProblem = new ProblemGenerator(params).generate(cspom)._1
  val concreteSolver = concrete.Solver(concreteProblem)

  // Members declared in 	 javax.constraints.impl.search.AbstractSolver
  def applySolution(x$1: javax.constraints.Solution): Boolean = ???

  private def concreteSol(s: Map[Variable, Any]) = {
    new Solution(
      s.mapValues(_.asInstanceOf[Int]))
  }

  def findSolution(ps: ProblemState): javax.constraints.Solution = {
    val s = concreteSolver.toIterable.headOption.map(concreteSol).getOrElse(null)
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
    })
      .map(concreteSol)
      .toArray

  override def solutionIterator: SolutionIterator = new SolutionIterator {
    val itr = concreteSolver
    def hasNext = itr.hasNext
    def next = concreteSol(itr.next)
  }

  def newSearchStrategy(): javax.constraints.SearchStrategy = {
    val ss = new SearchStrategy(this, params)
    //concreteSolver.asInstanceOf[MAC].setHeuristic(ss)
    ss
  }

  override def findOptimalSolution(objective: Objective, variable: Var): Solution = {
    objective match {
      case Objective.MAXIMIZE => ??? //concreteSolver.maximize(variable.getName)
      case Objective.MINIMIZE => ??? //concreteSolver.minimize(variable.getName)
    }
    concreteSolver.toIterable.lastOption.map(concreteSol).getOrElse(null)

  }

  // Members declared in javax.constraints.Solver 
  def trace(x$1: javax.constraints.VarSet): Unit = ???
  def trace(x$1: Array[javax.constraints.Var]): Unit = ???
  def trace(x$1: javax.constraints.Var): Unit = ???

}