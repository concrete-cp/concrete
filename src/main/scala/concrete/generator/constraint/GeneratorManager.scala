package concrete.generator.constraint;

import concrete.generator.FailedGenerationException
import concrete.Problem
import cspom.CSPOMConstraint;
import scala.collection.mutable.HashMap

final class GeneratorManager(val problem: Problem) {

  var generators: HashMap[Class[_ <: AbstractGenerator], AbstractGenerator] = new HashMap();

  @throws(classOf[FailedGenerationException])
  def generate(constraint: CSPOMConstraint): Boolean = {
    val candidate = GeneratorManager.known.getOrElse(constraint.function,
      throw new FailedGenerationException(s"No candidate constraint for $constraint"))

    generators.getOrElseUpdate(candidate, candidate.getConstructor(classOf[Problem]).newInstance(problem)).generate(constraint)
  }
}

object GeneratorManager {
  var known: Map[Symbol, Class[_ <: AbstractGenerator]] = Map(
    'abs -> classOf[AbsGenerator],
    'add -> classOf[AddGenerator],
    'sub -> classOf[AddGenerator],
    'allDifferent -> classOf[AllDifferentGenerator],
    'or -> classOf[DisjGenerator],
    'not -> classOf[DisjGenerator],
    'eq -> classOf[EqGenerator],
    'neg -> classOf[EqGenerator],
    'gt -> classOf[GtGenerator],
    'ge -> classOf[GtGenerator],
    'mul -> classOf[MulGenerator],
    'ne -> classOf[NeqGenerator],
    'absdiff -> classOf[AbsDiffGenerator],
    'diffge -> classOf[DiffGeGenerator],
    'gcc -> classOf[GccGenerator],
    'mod -> classOf[ModGenerator],
    'nevec -> classOf[NeqVecGenerator],
    'sum -> classOf[SumGenerator],
    'lexleq -> classOf[LexLeqGenerator],
    'occurrence -> classOf[OccurrenceGenerator],
    'extension -> classOf[ExtensionGenerator],
    'sq -> classOf[SquareGenerator])

  def register(entry: (Symbol, Class[_ <: AbstractGenerator])) {
    known += entry
  }
}
