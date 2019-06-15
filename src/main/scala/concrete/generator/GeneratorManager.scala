package concrete
package generator

import concrete.constraint.Constraint
import cspom.variable.CSPOMVariable
import cspom.{CSPOM, CSPOMConstraint, UNSATException}

import scala.util.{Failure, Success, Try}

class GeneratorManager(pg: ProblemGenerator) {

  var known: Map[String, Generator] = {
    val sg = new SumGenerator(pg)
    val adg = new AllDifferentGenerator(pg)
    Map(
      "abs" -> new AbsGenerator(pg),
      "absdiff" -> new AbsDiffGenerator(pg),
      "alldifferent" -> adg,
      "eq" -> new EqGenerator(pg.pm),
      "ne" -> new NeGenerator(pg),
      "mul" -> new MulGenerator(pg),
      "div" -> new DivGenerator(pg),
      "mod" -> new ModGenerator(pg),
      "nevec" -> new NeqVecGenerator(pg),
      "sum" -> sg,
      "pseudoboolean" -> sg,
      "lexleq" -> new LexLeqGenerator(pg),
      "atLeast" -> new AtLeastGenerator(pg),
      "atMost" -> new AtMostGenerator(pg),
      "extension" -> new ExtensionGenerator(pg),
      "sq" -> new SquareGenerator(pg),
      "min" -> new MinGenerator(pg),
      "max" -> new MaxGenerator(pg),
      "member" -> new MemberGenerator(pg),
      "element" -> new ElementGenerator(pg),
      "in" -> new SetInGenerator(pg),
      "circuit" -> new CircuitGenerator(adg),
      "xor" -> new XorGenerator(pg),
      "inverse" -> new InverseGenerator(pg, adg),
      "channel" -> new ChannelGenerator(pg, adg),
      "cumulative" -> new CumulativeGenerator(pg),
      "diffn" -> new DiffNGenerator(pg),
      "clause" -> ClauseGenerator,
      "bin_packing_load" -> new BinPackingGenerator(pg),
      "if" -> new IfGenerator(pg),
      "fzSubcircuit" -> new SubCircuitGenerator(pg),
    )
  }

  def generate[A](constraint: CSPOMConstraint[A], variables: Map[CSPOMVariable[_], Variable], cspom: CSPOM): Try[Seq[Constraint]] = {
    known.get(constraint.function).map(Success(_))
      .getOrElse(Failure(new FailedGenerationException(s"No candidate constraint for $constraint")))
      .flatMap { candidate =>
        Try {
          candidate.generate(constraint, variables: Map[CSPOMVariable[_], Variable])
        }
          .recoverWith {
            case e: UNSATException => Failure(e)
            case e =>
              Failure(new FailedGenerationException("Failed to generate " + constraint.toString(cspom.displayName), e))
          }
      }
  }

}
