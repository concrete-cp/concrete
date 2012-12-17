package cspfj

import cspfj.constraint.Constraint

final object AdviseCount {
  var count = 0
  def clear() {
    count += 1
  }

  def adviseAll(c: Constraint) {
    var p = c.arity - 1
    while (p >= 0) {
      c.advise(p)
      p -= 1
    }
  }
}
