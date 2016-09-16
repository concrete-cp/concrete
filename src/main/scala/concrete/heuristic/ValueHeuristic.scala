package concrete
package heuristic;

trait ValueHeuristic extends BranchHeuristic {
  def branch(variable: Variable, domain: Domain, ps: ProblemState): Branch = {
    val selected = selectIndex(variable, domain)
    val dom = ps.dom(variable)
    require(!dom.isAssigned && dom.present(selected))
    val b1 = dom.assign(selected)
    val b2 = dom.remove(selected)

    new Branch(
      ps.updateDomNonEmptyNoCheck(variable, b1), Seq((variable, Assignment)),
      ps.updateDomNonEmptyNoCheck(variable, b2), Seq((variable, InsideRemoval(dom, b2))),
      s"${variable.toString(ps)} = $selected",
      s"${variable.toString(ps)} /= $selected")
  }

  def selectIndex(variable: Variable, domain: Domain): Int
}

trait BranchHeuristic {
  def branch(variable: Variable, domain: Domain, ps: ProblemState): Branch

  def compute(problem: Problem): Unit

  def shouldRestart: Boolean
}