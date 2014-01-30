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
import Generator.restrictDomain

import Generator._

final object EqGenerator extends Generator {

  override def gen(constraint: CSPOMConstraint)(implicit variables: VarMap): Option[Seq[Constraint]] = {
    val Seq(a, b) = constraint.arguments.map(cspom2concrete1D)
    val neg: Boolean = constraint.getParam("neg", classOf[Boolean]).getOrElse(false)
    val negFactor = if (neg) -1 else 1
    val offset: Int = constraint.getParam("offset", classOf[Integer]).map(_.toInt).getOrElse(0)

    (a, b) match {
      case (Const(a), Const(b)) =>
        if (negFactor * a + offset == b) {
          Some(Seq())
        } else {
          throw UNSATObject
        }
      case (Var(a), Const(b)) =>
        restrictDomain(a, Seq((b - offset) * negFactor))
        Some(Seq())
      case (Const(a), Var(b)) =>
        restrictDomain(b, Seq(negFactor * a + offset))
        Some(Seq())
      case (Var(a), Var(b)) =>
        if (a.dom.undefined && b.dom.undefined) {
          None
        } else {
          if (!a.dom.undefined) {
            restrictDomain(b, a.dom.values.map(a => negFactor * a + offset))
          }
          if (!b.dom.undefined) {
            restrictDomain(a, b.dom.values.map(b => (b - offset) * negFactor))
          }
          if (a.dom.isInstanceOf[BooleanDomain] && b.dom.isInstanceOf[BooleanDomain]) {
            require(!neg && offset == 0)
            Some(Seq(new Disjunction(Array(a, b), Array(false, true)),
              new Disjunction(Array(b, a), Array(false, true))))
          } else {
            Some(Seq(new Eq(neg, a, offset, b)))
          }
        }
    }

  }

  override def genFunctional(funcConstraint: CSPOMConstraint, r: C2Conc)(implicit variables: VarMap): Option[Seq[Constraint]] = {
    val Var(result) = r
    Generator.booleanDomain(result)
    funcConstraint match {

      case CSPOMConstraint(_, 'eq, args, _) =>
        val Seq(a, b) = args map cspom2concrete1D

        if (undefinedVar(a, b).nonEmpty) {
          None
        } else {
          Generator.booleanDomain(result);
          (a, b) match {
            case (Const(a), Const(b)) =>
              if (a == b) {
                result.dom.setSingle(1)
              } else {
                result.dom.setSingle(0)
              }
              Some(Seq())
            case (Const(a), Var(b)) => Some(Seq(new ReifiedEquals(result, b, a)))
            case (Var(a), Const(b)) => Some(Seq(new ReifiedEquals(result, a, b)))
            case (Var(a), Var(b)) => Some(Seq(
              new ReifiedConstraint(
                result,
                new Eq(a, b),
                new Neq(a, b))))
          }

        }

      case CSPOMConstraint(_, 'neq, args, _) if args.size == 2 =>
        val scope = funcConstraint.arguments map cspom2concreteVar

        scope map { _.dom } find { !_.undefined } map { refDomain =>
          val negDomain = refDomain.values.map(v => -v).toSeq.reverse

          for (v <- scope if (v.dom.undefined)) {
            v.dom = IntDomain(negDomain: _*)
          }
          Seq(new Eq(true, scope(0), 0, scope(1)))

        }

      case _ => None
    }
  }
}
