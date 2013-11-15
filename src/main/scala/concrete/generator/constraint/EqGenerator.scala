package concrete.generator.constraint;

import concrete.constraint.semantic.{ ReifiedConstraint, Neq, Eq }
import concrete.generator.FailedGenerationException
import concrete.{ Variable, Problem, Domain, IntDomain }
import cspom.CSPOMConstraint
import concrete.BooleanDomain
import concrete.constraint.Constraint
import concrete.constraint.semantic.Disjunction
import concrete.UNSATObject
import concrete.constraint.semantic.ReifiedEquals
import AbstractGenerator.restrictDomain

final class EqGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def gen(constraint: CSPOMConstraint): Boolean = {
    val Seq(a, b) = constraint.arguments.map(cspom2concrete1D)

    (a, b) match {
      case (Const(a), Const(b)) => a == b || (throw UNSATObject)
      case (Var(a), Const(b)) =>
        restrictDomain(a, Seq(b))
        true
      case (Const(a), Var(b)) =>
        restrictDomain(b, Seq(a))
        true
      case (Var(a), Var(b)) =>

        if (a.dom.undefined && b.dom.undefined) {
          false
        } else {
          if (!a.dom.undefined) {
            restrictDomain(b, a.dom)
          }
          if (!b.dom.undefined) {
            restrictDomain(a, b.dom)
          }
          if (a.dom.isInstanceOf[BooleanDomain] && b.dom.isInstanceOf[BooleanDomain]) {
            addConstraint(new Disjunction(Array(a, b), Array(false, true)))
            addConstraint(new Disjunction(Array(b, a), Array(false, true)))
          } else {
            addConstraint(new Eq(a, b))
          }
          true
        }
    }

  }

  private def generateInt(refDomain: IntDomain, scope: Seq[Variable]) {
    for (v <- scope if (v.dom.undefined)) {
      v.dom = IntDomain(refDomain.values.toSeq: _*)
    }
    for (Seq(v0, v1) <- scope.sliding(2)) {
      addConstraint(new Eq(v0, v1));
    }
  }

  private def generateBool(refDomain: BooleanDomain, scope: Seq[Variable]) {
    scope filter { _.dom.undefined } map { _.dom.asInstanceOf[BooleanDomain] } find { _.size == 1 } match {
      case None =>
        for (v <- scope if (v.dom.undefined)) {
          v.dom = new BooleanDomain(refDomain.status)
        }

        for (Seq(i, j) <- scope.combinations(2)) {
          addConstraint(new Disjunction(Array(i, j), Array(false, true)))
        }

      case Some(constantDomain) =>
        for (v <- scope) {
          if (v.dom.undefined) {
            v.dom = new BooleanDomain(refDomain.status)
          } else {
            val d = v.dom.asInstanceOf[BooleanDomain]
            if (d.isUnknown) {
              d.status = refDomain.status
            } else if (d.status != refDomain.status) {
              throw new FailedGenerationException("Inconsistent equality")
            }
          }
        }

    }

  }

  override def genReified(funcConstraint: CSPOMConstraint, result: Variable) = funcConstraint match {
    case CSPOMConstraint(_, 'eq, args, _) =>
      val Seq(a, b) = args map cspom2concrete1D

      if (a.undefined || b.undefined) {
        false
      } else {
        AbstractGenerator.booleanDomain(result);
        (a, b) match {
          case (Const(a), Const(b)) =>
            if (a == b) {
              result.dom.setSingle(1)
            } else {
              result.dom.setSingle(0)
            }
          case (Const(a), Var(b)) => addConstraint(new ReifiedEquals(result, b, a))
          case (Var(a), Const(b)) => addConstraint(new ReifiedEquals(result, a, b))
          case (Var(a), Var(b)) => addConstraint(
            new ReifiedConstraint(
              result,
              new Eq(a, b),
              new Neq(a, b)))
        }

        true
      }

    case CSPOMConstraint(_, 'neq, args, _) if args.size == 2 =>
      val scope = funcConstraint.arguments map cspom2concreteVar

      scope map { _.dom } find { !_.undefined } match {
        case None => false
        case Some(refDomain) =>
          val negDomain = refDomain.values.map(v => -v).toSeq.reverse

          for (v <- scope if (v.dom.undefined)) {
            v.dom = IntDomain(negDomain: _*)
          }
          addConstraint(new Eq(true, scope(0), 0, scope(1)))
          true
      }

    case _ => false
  }

}
