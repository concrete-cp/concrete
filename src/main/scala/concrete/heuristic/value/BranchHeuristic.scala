package concrete
package heuristic.value

import java.util.EventObject

import concrete.heuristic.Branch

trait ValueHeuristic extends BranchHeuristic {
  def branch(variable: Variable, domain: Domain, ps: ProblemState): Branch = {
    assignBranch(ps, variable, domain, selectIndex(variable, domain))
  }

  def selectIndex(variable: Variable, domain: Domain): Int

}

trait BranchHeuristic {
  def assignBranch(ps: ProblemState, variable: Variable, dom: Domain, selected: Int): Branch = {
    require(!dom.isAssigned && dom.present(selected))
    val b1 = dom.assign(selected)
    val b2 = dom.remove(selected)

    new Branch(
      ps.updateDomNonEmptyNoCheck(variable, b1), Seq((variable, Assignment)),
      ps.updateDomNonEmptyNoCheck(variable, b2), Seq((variable, InsideRemoval(dom, b2))),
      s"${variable.toString(ps)} = $selected",
      s"${variable.toString(ps)} /= $selected")
  }

  def branch(variable: Variable, domain: Domain, ps: ProblemState): Branch

  def compute(solver: MAC, ps: ProblemState): ProblemState

  def shouldRestart: Boolean

  def event(e: EventObject): Unit = ()
}