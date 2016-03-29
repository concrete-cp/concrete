package concrete.generator;

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import concrete.ParameterManager
import concrete.Variable
import concrete.constraint.Constraint
import cspom.CSPOMConstraint
import cspom.VariableNames
import cspom.variable.CSPOMVariable

class GeneratorManager(pm: ParameterManager) {

  private var known: Map[Symbol, Generator] = {
    val sg = new SumGenerator(pm)
    val adg = new AllDifferentGenerator(pm)
    Map(
      'abs -> AbsGenerator,
      'alldifferent -> adg,
      'eq -> EqGenerator,
      //'gt -> GtGenerator,
      //'ge -> GtGenerator,
      'mul -> MulGenerator,
      'absdiff -> AbsDiffGenerator,
      'gcc -> GccGenerator,
      'div -> DivGenerator,
      'mod -> ModGenerator,
      'nevec -> NeqVecGenerator,
      'sum -> sg,
      'pseudoboolean -> sg,
      'lexleq -> LexLeqGenerator,
      'occurrence -> OccurrenceGenerator,
      'extension -> new ExtensionGenerator(pm),
      'sq -> SquareGenerator,
      'min -> MinGenerator,
      'max -> MaxGenerator,
      'element -> ElementGenerator,
      'in -> SetInGenerator,
      'circuit -> new CircuitGenerator(adg),
      'xor -> XorGenerator)
  }

  def register(entry: (Symbol, Generator)) {
    known += entry
  }

  def generate[A](constraint: CSPOMConstraint[A], variables: Map[CSPOMVariable[_], Variable], vn: VariableNames): Try[Seq[Constraint]] = {
    known.get(constraint.function).map(Success(_))
      .getOrElse(Failure(new FailedGenerationException(s"No candidate constraint for $constraint")))
      .flatMap { candidate =>
        Try {
          candidate.generate(constraint, variables: Map[CSPOMVariable[_], Variable])
        }
          .recoverWith {
            case e =>
              Failure(new FailedGenerationException("Failed to generate " + constraint.toString(vn), e))
          }
      }
  }

}
