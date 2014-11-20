package javax.constraints.impl

import cspom.variable.IntVariable
import concrete.Variable

class Var(problem: Problem, name: String, val variable: IntVariable) extends AbstractVar(problem, name) {
  setImpl(variable)

  var concreteVar: Option[Variable] = None

  // Members declared in javax.constraints.impl.AbstractVar 
  def addPropagator(x$1: javax.constraints.extra.Propagator, x$2: javax.constraints.extra.PropagationEvent) {
    throw new UnsupportedOperationException
  }

  // Members declared in javax.constraints.Var 
  def abs(): javax.constraints.Var = {
    val cspomVar = problem.cspom.is('abs, Seq(variable), Map()).asInstanceOf[IntVariable]
    new Var(problem, Var.generate(), cspomVar)
  }

  def contains(x: Int): Boolean = variable.contains(x)

  def getMax(): Int = concreteVar.map(_.initDomain.last).getOrElse(variable.asSortedSet.last)

  def getMin(): Int = concreteVar.map(_.initDomain.head).getOrElse(variable.asSortedSet.head)

  def isBound(): Boolean = concreteVar.map(_.initDomain.size == 1).getOrElse {
    throw new IllegalStateException("Solver was not created or variable is unavailable")
  }

  def multiply(v2: javax.constraints.Var): javax.constraints.Var = {
    ???
    //    val cspomVar = variable.*(v2.getImpl.asInstanceOf[IntVariable])(problem.cspom)
    //    new Var(problem, cspomVar.name, cspomVar)
  }

  def multiply(constant: Int): javax.constraints.Var = {
    ???
    //    val cspomVar = variable.*(problem.cspom.IntVariable.of(constant))(problem.cspom)
    //    new Var(problem, cspomVar.name, cspomVar)
  }

  def plus(v2: javax.constraints.Var): javax.constraints.Var = {
    ???
    //    val cspomVar = variable.+(v2.getImpl.asInstanceOf[IntVariable])(problem.cspom)
    //    new Var(problem, cspomVar.name, cspomVar)
  }

  def plus(constant: Int): javax.constraints.Var = {
    ???
    //    val cspomVar = variable.+(problem.cspom.IntVariable.of(constant))(problem.cspom)
    //    new Var(problem, cspomVar.name, cspomVar)
  }
}

object Var {
  var unnamed = 0;

  /**
   * Generates an unique variable name.
   *
   * @return An unique variable name.
   */
  def generate() = {
    val name = "_" + unnamed;
    unnamed += 1;
    name;
  }
}