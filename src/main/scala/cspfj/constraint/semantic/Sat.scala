package cspfj.constraint.semantic
import org.sat4j.minisat.SolverFactory
import org.sat4j.specs.IProblem
import cspfj.problem.Variable
import org.sat4j.core.VecInt
import java.io.PrintWriter
import cspfj.constraint.Residues
import cspfj.constraint.AbstractConstraint
import cspfj.constraint.VariablePerVariable

class Sat(scope: IndexedSeq[Variable]) extends AbstractConstraint(null, scope.toArray)
  with Residues with VariablePerVariable {

  val offset = scope.map(_.dom.maxSize).scanLeft(1)(_ + _).toIndexedSeq

  private def numVariable(variablePosition: Int, index: Int) =
    index + offset(variablePosition)

  val values = (0 until scope.size) map { vp =>
    offset(vp) until offset(vp + 1) toArray
  }

  var clauses: List[Array[Int]] = List.empty

  def newSolver = {
    val MAXVAR = scope.map(_.dom.maxSize).sum
    //val NBCLAUSES = 500000;

    val s = SolverFactory.newLight();

    // prepare the solver to accept MAXVAR variables. MANDATORY
    s.newVar(MAXVAR);
    // not mandatory for SAT solving. MANDATORY for MAXSAT solving
    // solver.setExpectedNumberOfClauses(NBCLAUSES);
    // Feed the solver using Dimacs format, using arrays of int
    // (best option to avoid dependencies on SAT4J IVecInt)

    values foreach { (v => s.addAtLeast(new VecInt(v), 1)) }
    clauses foreach { (c => s.addClause(new VecInt(c))) }
    s
  }

  def this(scope: Variable*) = this(scope.toIndexedSeq)

  override def check() = {
    val s = newSolver

    for (
      vp <- 0 until scope.size
    ) {
      s.addClause(new VecInt(Array(numVariable(vp, tuple(vp)))))
    }

    s.isSatisfiable
  }

  def noGoodVP(values: Seq[(Int, Int)]) {
    clauses ::= values.map(v => -numVariable(v._1, v._2)).toArray
  }

  def noGood(values: Seq[(Variable, Int)]) {
    noGoodVP(values.map(v => (position(v._1), v._2)))
  }

  override def findSupport(vp: Int, index: Int) = {
    val s = newSolver
    //println(clauses map (_.toSeq))
    for (
      p <- (0 until arity) if (p != vp);
      i <- (0 until scope(p).dom.maxSize) if (!scope(p).dom.present(i))
    ) {
      //println(-numVariable(p, i))
      s.addClause(new VecInt(Array(-numVariable(p, i))))
    }
    //println(numVariable(vp, index))

    s.addClause(new VecInt(Array(numVariable(vp, index))))

    if (s.isSatisfiable) {
      println(s.model.toSeq)
      true
    } else {
      false
    }
  }

  override def getEvaluation = 1 << (scope.map(_.dom.size).sum / 100)
}