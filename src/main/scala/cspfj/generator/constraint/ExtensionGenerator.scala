package cspfj.generator.constraint;

import com.google.common.base.Function
import com.google.common.base.Objects
import com.google.common.collect.Iterables
import com.google.common.collect.Lists
import com.google.common.collect.Maps
import com.google.common.primitives.Ints
import cspfj.constraint.extension.ExtensionConstraints
import cspfj.constraint.extension.Matrix
import cspfj.constraint.extension.Matrix2D
import cspfj.constraint.extension.MatrixGeneral
import cspfj.constraint.extension.TupleSet
import cspfj.exception.FailedGenerationException
import cspfj.problem.Domain
import cspfj.problem.Problem
import cspfj.problem.Variable
import cspom.constraint.CSPOMConstraint
import cspom.xcsp.Extension
import scala.collection.JavaConversions

final case class Signature(domains: Seq[Domain], extension: Extension)

final class ExtensionGenerator(problem: Problem) extends AbstractGenerator(problem) {

  var generated: Map[Signature, Matrix] = Map.empty

  private def generate(variables: Seq[Variable], extension: Extension) = {
    val domains = variables map (_.getDomain)

    val signature = Signature(domains, extension);
    generated.get(signature) match {
      case None => {
        val matrix = ExtensionGenerator.bestMatrix(extension, domains map (_.size))
        ExtensionGenerator.fillMatrix(domains, extension, matrix)
        generated += signature -> matrix
        matrix
      }
      case Some(m) => m
    }

  }

  def generate(constraint: CSPOMConstraint) = {
    require(constraint.isInstanceOf[cspom.extension.ExtensionConstraint])

    val solverVariables = constraint.scope map getSolverVariable

    if (solverVariables exists (_.getDomain == null)) {
      false
    } else {
      val matrix = generate(solverVariables, constraint.asInstanceOf[cspom.extension.ExtensionConstraint].extension);

      addConstraint(ExtensionConstraints.newExtensionConstraint(matrix, solverVariables: _*));
      true;
    }
  }
}

object ExtensionGenerator {
  val TIGHTNESS_LIMIT = 4;
  
  def tupleSetBetterThanMatrix(sizes: Seq[Int], nbTuples: Int) = {
    val size = sizes.foldLeft(BigInt(1))(_ * _)
    size > Int.MaxValue || size > (TIGHTNESS_LIMIT * nbTuples)
  }

  def bestMatrix(extension: Extension, sizes: Seq[Int]) = {
    if (extension.relation.arity == 2) {
      new Matrix2D(sizes(0), sizes(1), extension.init);
    } else if (!extension.init && tupleSetBetterThanMatrix(sizes, extension.relation.size)) {
      new TupleSet(extension.relation.size, extension.init);
    } else {
      new MatrixGeneral(sizes.toArray, extension.init);
    }
  }

  def fillMatrix(domains: Seq[Domain], extension: Extension, matrix: Matrix) {

    val indices = domains map (d =>
      JavaConversions.asScalaIterator(d.iterator) map (i => i -> d.value(i)) toMap);

    //        final int[] tuple = new int[domains.size()];
    //        final List<Map<Number, Integer>> indexes = Lists.transform(domains,
    //                new Function<Domain, Map<Number, Integer>>() {
    //                    @Override
    //                    public Map<Number, Integer> apply(final Domain domain) {
    //                        return Maps.uniqueIndex(domain,
    //                                new Function<Integer, Number>() {
    //                                    @Override
    //                                    public Number apply(Integer input) {
    //                                        return domain.value(input);
    //                                    }
    //                                });
    //                    }
    //                });

    for (values <- extension.relation.asInstanceOf[Set[Seq[Int]]]) {
      val tuple = indices.zip(values).map(t => t._1(t._2)).toArray
      matrix.set(tuple, !extension.init)
    }
    //
    //        for (Number[] values : extension.getTuples()) {
    //            for (int i = tuple.length; --i >= 0;) {
    //                tuple[i] = indexes.get(i).get(values[i]);
    //            }
    //            matrix.set(tuple, !extension.init());
    //        }

  }
}
