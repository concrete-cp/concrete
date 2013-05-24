package javax.constraints.impl.search

class Solver(problem: javax.constraints.impl.Problem) extends javax.constraints.impl.search.AbstractSolver(problem) {
  val cspom = problem.cspom
  val concreteSolver = concrete.Solver(cspom)

  // Members declared in 	 javax.constraints.impl.search.AbstractSolver
  def applySolution(x$1: javax.constraints.Solution): Boolean = ???

  def findSolution(x$1: javax.constraints.ProblemState): javax.constraints.Solution = 
    concreteSolver.toIterable.headOption.map(new Solution(_)).getOrElse(null)


  def newSearchStrategy(): javax.constraints.SearchStrategy = ???

  // Members declared in javax.constraints.Solver 
  def trace(x$1: javax.constraints.VarSet): Unit = ???
  def trace(x$1: Array[javax.constraints.Var]): Unit = ???
  def trace(x$1: javax.constraints.Var): Unit = ???

}