package concrete

import concrete.constraint.Constraint

final class ProblemState(
  val domains: IndexedSeq[Domain],
  val constraintStates: IndexedSeq[Any],
  val entailed: Set[Int]) {

  def constraintState(id: Int): Any = constraintStates(id)

  def apply(c: Constraint): c.State = constraintState(c.id).asInstanceOf[c.State]

  def updatedCS(id: Int, newState: Any): ProblemState = new ProblemState(domains, constraintStates.updated(id, newState), entailed)

  def updatedCS(c: Constraint, newState: Any): ProblemState = updatedCS(c.id, newState)

  def updateConstraints(f: (Int, Any) => Any) = {
    var i = constraintStates.length - 1
    val builder = constraintStates.genericBuilder[Any]
    builder.sizeHint(constraintStates.length)
    while (i < constraintStates.length) {
      builder += f(i, constraintStates(i))
    }
    new ProblemState(domains, builder.result, entailed)
  }

  def entail(c: Constraint): ProblemState = new ProblemState(domains, constraintStates, entailed + c.id)
  def isEntailed(c: Constraint): Boolean = entailed(c.id)

  def updatedDomain(id: Int, newDomain: Domain): ProblemState =
    if (domains(id) eq newDomain) { this }
    else { new ProblemState(domains.updated(id, newDomain), constraintStates, entailed) }

  def updatedDomain(v: Variable, newDomain: Domain): ProblemState =
    updatedDomain(v.id, newDomain)

  def updatedDomains(v: Array[Variable], newDomains: IndexedSeq[Domain]): ProblemState = {
    var i = v.length - 1
    var d = domains
    while (i >= 0) {
      val id = v(i).id
      val nd = newDomains(i)
      if (d(id) ne nd) {
        d = d.updated(id, nd)
      }
      i -= 1
    }
    if (d ne domains) new ProblemState(d, constraintStates, entailed) else this
  }

  def domain(id: Int): Domain = domains(id)

  def apply(v: Variable): Domain = domain(v.id)

  def assign(variable: Variable, value: Int): ProblemState = {
    val id = variable.id
    updatedDomain(id, domains(id).assign(value))
  }

  def remove(variable: Variable, value: Int): ProblemState = {
    val id = variable.id
    val removed = domain(id).remove(value)
    updatedDomain(id, removed)
  }

  def assign(p: Pair): ProblemState = assign(p.variable, p.value)
  def remove(p: Pair): ProblemState = remove(p.variable, p.value)

  def domains(v: Array[Variable]) = v.map(apply)
}