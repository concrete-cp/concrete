package concrete.generator

import Generator.cspom2concrete
import Generator.cspom2concreteSeq
import concrete.constraint.semantic.Member
import concrete.constraint.semantic.NotMember
import cspom.CSPOMConstraint
import concrete.constraint.ReifiedConstraint

class MemberGenerator(pg: ProblemGenerator) extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit varMap: VarMap) = {
    val seq = cspom2concreteSeq(constraint.arguments(0)).map(_.asVariable(pg))
    val variable = cspom2concrete(constraint.arguments(1)).asVariable(pg)
    Seq(new Member(variable, seq.toArray))
  }

  override def genReversed(constraint: CSPOMConstraint[Boolean])(implicit varMap: VarMap) = {
    val seq = cspom2concreteSeq(constraint.arguments(0)).map(_.asVariable(pg))
    val variable = cspom2concrete(constraint.arguments(1)).asVariable(pg)
    Seq(new NotMember(variable, seq.toArray))
  }

  override def genFunctional(constraint: CSPOMConstraint[_], result: C2Conc)(implicit varMap: VarMap) = {
    val seq = cspom2concreteSeq(constraint.arguments(0)).map(_.asVariable(pg)).toArray
    val variable = cspom2concrete(constraint.arguments(1)).asVariable(pg)
    val r = result.asVariable(pg)
    Seq(
      new ReifiedConstraint(neg = false, r, new Member(variable, seq)),
      new ReifiedConstraint(neg = true, r, new NotMember(variable, seq)))
  }

}