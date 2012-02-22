package cspfj.heuristic;

import scala.collection.JavaConversions
import cspfj.constraint.Constraint
import cspfj.problem.Problem
import cspfj.problem.Variable;
import scala.annotation.tailrec

final class DDegOnDom(val problem: Problem) extends VariableHeuristic with RandomBreak {

  def score(variable: Variable) =
    dDeg(variable).toDouble / variable.dom.size

  private def dDeg(variable: Variable) = {
    val constraints = variable.constraints

    @tailrec
    def sum(i: Int, s: Int): Int =
      if (i < 0) s
      else {
        val c = constraints(i)
        if (c.isEntailed) sum(i - 1, s)
        else sum(i - 1, s + 1)
      }

    sum(constraints.size - 1, 0)

  }

  override def toString = "max-ddeg/dom"

}
