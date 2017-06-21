package concrete
package generator;

import com.typesafe.scalalogging.LazyLogging

import Generator.cspom2concrete
import concrete.constraint.Constraint
import concrete.constraint.semantic.NeqReif
import cspom.CSPOMConstraint

final class NeGenerator(pg: ProblemGenerator) extends Generator with LazyLogging {

  override def genFunctional(funcConstraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap): Seq[Constraint] = {
    val result = r.asVariable(pg)
    val Seq(a, b) = funcConstraint.arguments.map(cspom2concrete(_)).map(_.asVariable(pg))

    Seq(new NeqReif(result, a, b))

  }

}
