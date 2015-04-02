package concrete.generator.cspompatterns

import cspom.compiler.Types
import cspom.variable.BoolVariable
import cspom.variable.FreeVariable

object ConcreteTypes extends Types {
  def types = Map(
    'clause -> { (c, p) =>
      c.fullScope.flatMap(_.flatten).collect {
        case fv: FreeVariable => fv
      } map { v =>
        replace(v, new BoolVariable, p)
      } reduce (_ ++ _)
    })
}