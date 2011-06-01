package cspfj.generator.constraint;

import cspfj.constraint.semantic.{ReifiedConstraint, Neq, Eq}
import cspfj.exception.FailedGenerationException
import cspfj.problem.{Variable, Problem, Domain, BitVectorDomain}
import cspom.constraint.{GeneralConstraint, FunctionalConstraint, CSPOMConstraint}

final class EqGenerator(problem: Problem) extends AbstractGenerator(problem) {

  private def generateGeneral(constraint: CSPOMConstraint) = {
    require("eq" == constraint.description);
    if (constraint.scope.size != 2) {
      null;
    } else {
      val scope = constraint.scope map cspom2cspfj

      val refDomain = scope map { _.getDomain } find { _ != null }

      if (refDomain == None) {
        null
      } else {
        for (v <- scope if (v.getDomain == null)) {
          v.setDomain(new BitVectorDomain(refDomain.get.allValues: _*))
        }
        new Eq(scope(0), scope(1));
      }
    }
  }

  private def generateReify(funcConstraint: FunctionalConstraint) = funcConstraint.description match {
    case "eq" => {
      val arguments = funcConstraint.arguments map cspom2cspfj

      if (arguments.size != 2 || arguments.exists(_.getDomain == null)) {
        null
      } else {
        val result = cspom2cspfj(funcConstraint.result)
        AbstractGenerator.booleanDomain(result);
        new ReifiedConstraint(result, new Eq(arguments(0),
          arguments(1)), new Neq(arguments(0),
          arguments(1)));
      }
    }
    case "neg" =>
      if (funcConstraint.scope.size != 2) {
        null
      } else {
        val scope = funcConstraint.scope map cspom2cspfj

        val refDomain = scope map { _.getDomain } find { _ != null }

        if (refDomain == None) {
          null
        } else {
          val negDomain = refDomain.get.allValues map { v => (-v) } reverse

          for (v <- scope if (v.getDomain == null)) {
            v.setDomain(new BitVectorDomain(negDomain: _*))
          }
          new Eq(-1, scope(0), 0, scope(1));
        }
      }

    case _ =>
      throw new IllegalArgumentException("Can not generate " + funcConstraint);
  }

  def generate(constraint: CSPOMConstraint) = {
    val generated = constraint match {
      case gC: GeneralConstraint => generateGeneral(gC)
      case fC: FunctionalConstraint => generateReify(fC)
      case _ => throw new FailedGenerationException(constraint + " not supported");
    }
    if (generated == null) {
      false
    } else {
      addConstraint(generated);
      true;
    }
  }

}
