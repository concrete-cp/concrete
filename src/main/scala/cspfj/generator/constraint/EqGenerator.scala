package cspfj.generator.constraint;

import cspfj.constraint.semantic.{ ReifiedConstraint, Neq, Eq }
import cspfj.generator.FailedGenerationException
import cspfj.{ Variable, Problem, Domain, IntDomain }
import cspom.constraint.{ GeneralConstraint, FunctionalConstraint, CSPOMConstraint }
import cspfj.BooleanDomain
import cspfj.constraint.Constraint
import cspfj.constraint.semantic.Disjunction

final class EqGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def generateGeneral(constraint: GeneralConstraint): Boolean = {
    require("eq" == constraint.description);
    if (constraint.scope.size != 2) false
    else {
      val scope = constraint.scope map cspom2cspfj

      scope map { _.dom } find { _ != null } match {
        case None => false
        case Some(refDomain: IntDomain) =>
          for (v <- scope if (v.dom == null)) {
            v.dom = IntDomain(refDomain.values.toSeq: _*)
          }
          addConstraint(new Eq(scope(0), scope(1)));
          true

        case Some(refDomain: BooleanDomain) =>
          scope filter { _.dom ne null } map { _.dom.asInstanceOf[BooleanDomain] } find { _.size == 1 } match {
            case None =>
              for (v <- scope if (v.dom == null)) {
                v.dom = new BooleanDomain(refDomain.status)
              }

              for (i <- scope; j <- scope if i != j) {
                addConstraint(new Disjunction(Array(i, j), Array(false, true)))
              }

              true

            case Some(constantDomain) =>
              for (v <- scope) {
                if (v.dom == null) {
                  v.dom = new BooleanDomain(refDomain.status)
                } else {
                  val d = v.dom.asInstanceOf[BooleanDomain]
                  if (d.isUnknown) d.status = refDomain.status
                  else if (d.status != refDomain.status) throw new FailedGenerationException("Inconsistent equality")
                }
              }
              true
          }

      }

    }
  }

  override def generateFunctional(funcConstraint: FunctionalConstraint) = funcConstraint.description match {
    case "eq" => {
      val arguments = funcConstraint.arguments map cspom2cspfj

      if (arguments.size != 2 || arguments.exists(_.dom == null)) {
        false
      } else {
        val result = cspom2cspfj(funcConstraint.result)
        AbstractGenerator.booleanDomain(result);
        addConstraint(
          new ReifiedConstraint(
            result,
            new Eq(arguments(0), arguments(1)),
            new Neq(arguments(0), arguments(1))))
        true
      }
    }
    case "neg" =>
      if (funcConstraint.scope.size != 2) {
        false
      } else {
        val scope = funcConstraint.scope map cspom2cspfj

        val refDomain = scope map { _.dom } find { _ != null }

        if (refDomain == None) {
          false
        } else {
          val negDomain = refDomain.get.values.map(v => -v).toSeq.reverse

          for (v <- scope if (v.dom == null)) {
            v.dom = IntDomain(negDomain: _*)
          }
          addConstraint(new Eq(true, scope(0), 0, scope(1)))
          true
        }
      }

    case _ =>
      throw new IllegalArgumentException("Can not generate " + funcConstraint);
  }

}
