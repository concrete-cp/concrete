package cspfj.generator.constraint;


import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspom.constraint.CSPOMConstraint;

final class GeneratorManager(val problem: Problem) {
  
 
	
	var  generators: Map[Class[Generator], Generator] = Map.empty;

	@throws(classOf[FailedGenerationException])
	def generate(constraint: CSPOMConstraint)={
	  val candidate = GeneratorManager.known.get(constraint.description.toLowerCase) match {
	    case None => throw new FailedGenerationException("No candidate constraint for "
                    + constraint + " (" + constraint.description + ")");
	    case Some(generator) => generator
	  }
	  
	  
	  (generators.get(candidate) match {
	    case Some(generator) => generator
	    case None => {
	      val generator = candidate.getConstructor(classOf[Problem]).newInstance(problem);
	      generators += candidate -> generator
	      generator
	    }
	  }).generate(constraint)
	}
}

object GeneratorManager {
   var known : Map[String, Class[Generator]] = Map(
      "abs"-> classOf[AbsGenerator],
        "add"-> classOf[AddGenerator],
        "sub"-> classOf[AddGenerator],
        "alldifferent"-> classOf[AllDifferentGenerator],
        "or"-> classOf[DisjGenerator],
        "not"-> classOf[DisjGenerator],
        "and"-> classOf[DisjGenerator],
        "eq"-> classOf[EqGenerator],
        "neg"-> classOf[EqGenerator],
        "ext"-> classOf[ExtensionGenerator],
        "gt"-> classOf[GtGenerator],
        "ge"-> classOf[GtGenerator],
        "lt"-> classOf[GtGenerator],
        "le"-> classOf[GtGenerator],
        "mul"-> classOf[MulGenerator],
        "ne"-> classOf[NeqGenerator],
        "absdiff"-> classOf[AbsDiffGenerator],
        "diffGe"-> classOf[DiffGeGenerator],
        "lexleq"-> classOf[LexLeqGenerator],
        "gcc"-> classOf[GccGenerator]
      )

  def register(entry: Pair[String, Class[Generator]]) {
    known += entry
  }
}
