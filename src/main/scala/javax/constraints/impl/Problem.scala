package javax.constraints.impl

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import cspom.Loggable
import concrete.generator.ProblemGenerator
import javax.constraints.impl.search.Solver
import javax.constraints.Oper
import cspom.variable.IntVariable

class Problem(name: String) extends AbstractProblem(name) with Loggable {
  def this() = this("")

  val cspom = new CSPOM()

  var concreteProblemOption: Option[concrete.Problem] = None

  def concreteProblem = concreteProblemOption.getOrElse {
    throw new IllegalStateException("Solver was not created")
  }

  // Members declared in javax.constraints.impl.AbstractProblem
  protected def createSolver(): javax.constraints.Solver = {
    val solver = new Solver(this)
    concreteProblemOption = Some(solver.concreteProblem)
    for (v <- getVars.map(_.asInstanceOf[javax.constraints.impl.Var])) {
      v.concreteVar = solver.concreteProblem.variableMap.get(v.getName)
    }
    solver
  }

  private def cspomVar(vars: Seq[javax.constraints.Var]) = vars.map(_.getImpl.asInstanceOf[IntVariable])

  def createVariable(name: String, min: Int, max: Int): javax.constraints.Var = {
    new Var(this, name, IntVariable(min to max))
  }
  def debug(l: String) { logger.fine(l) }
  def error(l: String) { logger.severe(l) }
  def getImplVersion(): String = "CSPOM JSR331 implementation"
  def log(l: String) { logger.info(l) }
  def post(constraint: javax.constraints.Constraint) {
    cspom.ctr(constraint.getImpl.asInstanceOf[CSPOMConstraint[_]])
  }
  def variableBool(name: String): javax.constraints.VarBool = ???

  // Members declared in javax.constraints.Problem
  def allDiff(scope: Array[javax.constraints.Var]): javax.constraints.Constraint = {
    val constraint = CSPOMConstraint('allDifferent, scope.map(_.getImpl.asInstanceOf[IntVariable]): _*)
    new Constraint(this, constraint)
  }
  def linear(v1: javax.constraints.Var, op: String, v2: javax.constraints.Var): javax.constraints.Constraint =
    post(v1, op, v2)

  def linear(v: javax.constraints.Var, op: String, c: Int): javax.constraints.Constraint =
    post(v, op, c)

  def loadFromXML(is: java.io.InputStream) {
    ???
  }
  def storeToXML(os: java.io.OutputStream, comments: String) {
    ???
//    val ow = new OutputStreamWriter(os)
//    xml.XML.write(ow, XCSPWriter(cspom), xml.XML.encoding, false, null)
//    ow.close()
  }
  def post(v1: javax.constraints.Var, op: String, v2: javax.constraints.Var): javax.constraints.Constraint = {
    val constraint = cspom.ctr(
      CSPOMConstraint(Symbol(op), v1.getImpl.asInstanceOf[CSPOMVariable[_]], v2.getImpl.asInstanceOf[CSPOMVariable[_]]))
    new Constraint(this, constraint)
  }
  def post(v: javax.constraints.Var, op: String, c: Int): javax.constraints.Constraint = {
    val constraint = cspom.ctr(
      CSPOMConstraint(Symbol(op), v.getImpl.asInstanceOf[CSPOMVariable[_]], IntVariable(Seq(c))))
    new Constraint(this, constraint)
  }
  def post(sum: Array[javax.constraints.Var], op: String, v: javax.constraints.Var): javax.constraints.Constraint = {
    ???
    //    val lb = sum.map(_.getMin()).sum
    //    val ub = sum.map(_.getMax()).sum
    //    val r = cspom.interVar(lb, ub)
    //    val c = cspom.ctr("zerosum", r +: sum.map(_.getImpl.asInstanceOf[CSPOMVariable]),
    //      Map("coefficients" -> (-1 :: List.fill(sum.length)(1))))
    //    val constraint = cspom.ctr(op, r, v.getImpl.asInstanceOf[CSPOMVariable])
    //    new Constraint(this, constraint)
  }
  def post(vs: Array[javax.constraints.Var], op: String, v: Int): javax.constraints.Constraint = {
    val constant = IntVariable(Seq(v))
    post(vs, op, new Var(this, Var.generate(), constant))
  }
  def post(x$1: Array[Int], x$2: Array[javax.constraints.Var], x$3: String, x$4: javax.constraints.Var): javax.constraints.Constraint = ???
  def post(x$1: Array[Int], x$2: Array[javax.constraints.Var], x$3: String, x$4: Int): javax.constraints.Constraint = ???
  def postCardinality(vars: Array[javax.constraints.Var], cardValue: Int, op: String, cardCount: javax.constraints.Var): javax.constraints.Constraint = {
    val count = cspom.isInt('occurrence, cspomVar(vars), Map("occurence" -> cardValue))
    val countVar = new Var(this, Var.generate(), count)
    post(countVar, op, cardCount)
  }
  def postCardinality(vars: Array[javax.constraints.Var], cardValue: Int, op: String, cardCount: Int): javax.constraints.Constraint = {
    val constant = IntVariable(Seq(cardCount))

    postCardinality(vars, cardValue, op, new Var(this, Var.generate(), constant))
  }
  def postElement(x$1: Array[javax.constraints.Var], x$2: javax.constraints.Var, x$3: String, x$4: javax.constraints.Var): javax.constraints.Constraint = ???
  def postElement(x$1: Array[javax.constraints.Var], x$2: javax.constraints.Var, x$3: String, x$4: Int): javax.constraints.Constraint = ???
  def postElement(x$1: Array[Int], x$2: javax.constraints.Var, x$3: String, x$4: javax.constraints.Var): javax.constraints.Constraint = ???
  def postElement(x$1: Array[Int], x$2: javax.constraints.Var, x$3: String, x$4: Int): javax.constraints.Constraint = ???
  def scalProd(x$1: Array[Int], x$2: Array[javax.constraints.Var]): javax.constraints.Var = ???

}