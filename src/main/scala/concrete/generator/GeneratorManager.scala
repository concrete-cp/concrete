package concrete.generator;

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import concrete.ParameterManager
import concrete.Variable
import concrete.constraint.Constraint
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.CSPOM

class GeneratorManager(pg: ProblemGenerator) {

  var known: Map[Symbol, Generator] = {
    val sg = new SumGenerator(pg)
    val adg = new AllDifferentGenerator(pg)
    Map(
      'abs -> AbsGenerator,
      'absdiff -> new AbsDiffGenerator(pg),
      'alldifferent -> adg,
      'eq -> new EqGenerator(pg.pm),
      'mul -> new MulGenerator(pg),
      'div -> new DivGenerator(pg),
      'mod -> new ModGenerator(pg),
      'nevec -> new NeqVecGenerator(pg),
      'sum -> sg,
      'pseudoboolean -> sg,
      'lexleq -> new LexLeqGenerator(pg),
      'atLeast -> new AtLeastGenerator(pg),
      'atMost -> new AtMostGenerator(pg),
      'extension -> new ExtensionGenerator(pg),
      'sq -> SquareGenerator,
      'min -> new MinGenerator(pg),
      'max -> new MaxGenerator(pg),
      'element -> new ElementGenerator(pg),
      'in -> new SetInGenerator(pg),
      'circuit -> new CircuitGenerator(adg),
      'xor -> new XorGenerator(pg),
      'inverse -> new InverseGenerator(pg, adg),
      'cumulative -> new CumulativeGenerator(pg),
      'diffn -> new DiffNGenerator(pg),
      'clause -> ClauseGenerator,
      'bin_packing_load -> new BinPackingGenerator(pg))
  }

  def generate[A](constraint: CSPOMConstraint[A], variables: Map[CSPOMVariable[_], Variable], cspom: CSPOM): Try[Seq[Constraint]] = {
    known.get(constraint.function).map(Success(_))
      .getOrElse(Failure(new FailedGenerationException(s"No candidate constraint for $constraint")))
      .flatMap { candidate =>
        Try {
          candidate.generate(constraint, variables: Map[CSPOMVariable[_], Variable])
        }
          .recoverWith {
            case e =>
              Failure(new FailedGenerationException("Failed to generate " + constraint.toString(cspom.displayName), e))
          }
      }
  }

}
