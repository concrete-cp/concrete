package concrete.generator.constraint;

import concrete.generator.FailedGenerationException
import concrete.Problem
import cspom.CSPOMConstraint
import scala.collection.mutable.HashMap
import concrete.constraint.Constraint
import cspom.variable.CSPOMVariable
import concrete.Variable

object GeneratorManager {
  private var known: Map[Symbol, Generator] = Map(
    'abs -> AbsGenerator,
    'add -> AddGenerator,
    'alldifferent -> AllDifferentGenerator,
    'allDifferent -> AllDifferentGenerator,
    'or -> DisjGenerator,
    'not -> DisjGenerator,
    'eq -> EqGenerator,
    'neg -> EqGenerator,
    'gt -> GtGenerator,
    'ge -> GtGenerator,
    'mul -> MulGenerator,
    'ne -> NeqGenerator,
    'absdiff -> AbsDiffGenerator,
    'diffge -> DiffGeGenerator,
    'gcc -> GccGenerator,
    'mod -> ModGenerator,
    'nevec -> NeqVecGenerator,
    'sum -> SumGenerator,
    'lexleq -> LexLeqGenerator,
    'occurrence -> OccurrenceGenerator,
    'extension -> ExtensionGenerator,
    'sq -> SquareGenerator)

  def register(entry: (Symbol, Generator)) {
    known += entry
  }

  @throws(classOf[FailedGenerationException])
  def generate[A](constraint: CSPOMConstraint[A], variables: Map[CSPOMVariable[_], Variable], problem: Problem): Option[Seq[Constraint]] = {
    val candidate = known.getOrElse(constraint.function,
      throw new FailedGenerationException(s"No candidate constraint for $constraint"))

    candidate.generate(constraint, variables: Map[CSPOMVariable[_], Variable], problem)
  }

}
