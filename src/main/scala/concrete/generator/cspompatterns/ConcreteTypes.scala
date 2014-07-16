package concrete.generator.cspompatterns

import cspom.compiler.Types
import cspom.variable.BoolVariable
import cspom.variable.FreeVariable

object ConcreteTypes extends Types {
  def types = Map(
    'or -> { (c, p) =>
      c.fullScope.collect {
        case fv: FreeVariable => fv
      } map { v =>
        replace(Seq(v), new BoolVariable, p)
      } reduce (_ ++ _)
    })
}