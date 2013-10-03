package concrete.generator.constraint;

import concrete.generator.FailedGenerationException
import concrete.Problem
import cspom.CSPOMConstraint;
import scala.collection.mutable.HashMap

final class GeneratorManager(val problem: Problem) {

  var generators: HashMap[Class[_ <: AbstractGenerator], AbstractGenerator] = new HashMap();

  @throws(classOf[FailedGenerationException])
  def generate(constraint: CSPOMConstraint): Boolean = {
    val candidate = GeneratorManager.known.getOrElse(constraint.function.toLowerCase,
      throw new FailedGenerationException(s"No candidate constraint for $constraint"))

    generators.getOrElseUpdate(candidate, candidate.getConstructor(classOf[Problem]).newInstance(problem)).generate(constraint)
  }
}

object GeneratorManager {
  var known: Map[String, Class[_ <: AbstractGenerator]] = Map(
    "abs" -> classOf[AbsGenerator],
    "add" -> classOf[AddGenerator],
    "sub" -> classOf[AddGenerator],
    "alldifferent" -> classOf[AllDifferentGenerator],
    "or" -> classOf[DisjGenerator],
    "not" -> classOf[DisjGenerator],
    "and" -> classOf[DisjGenerator],
    "eq" -> classOf[EqGenerator],
    "=" -> classOf[EqGenerator],
    "neg" -> classOf[EqGenerator],
    "ext" -> classOf[ExtensionGenerator],
    "gt" -> classOf[GtGenerator],
    "ge" -> classOf[GtGenerator],
    "lt" -> classOf[GtGenerator],
    "le" -> classOf[GtGenerator],
    ">" -> classOf[GtGenerator],
    ">=" -> classOf[GtGenerator],
    "<" -> classOf[GtGenerator],
    "<=" -> classOf[GtGenerator],
    "mul" -> classOf[MulGenerator],
    "ne" -> classOf[NeqGenerator],
    "absdiff" -> classOf[AbsDiffGenerator],
    "diffge" -> classOf[DiffGeGenerator],
    "gcc" -> classOf[GccGenerator],
    "mod" -> classOf[ModGenerator],
    "nevec" -> classOf[NeqVecGenerator],
    "zerosum" -> classOf[SumGenerator],
    "lexleq" -> classOf[LexLeqGenerator],
    "occurrence" -> classOf[OccurrenceGenerator],
    "extension" -> classOf[ExtensionGenerator],
    "sq" -> classOf[SquareGenerator])

  def register(entry: (String, Class[_ <: AbstractGenerator])) {
    known += entry
  }
}
