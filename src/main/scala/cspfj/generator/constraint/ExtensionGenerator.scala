package cspfj.generator.constraint;

import scala.Array.canBuildFrom
import scala.math.BigInt.int2bigInt
import cspfj.constraint.extension.ExtensionConstraint
import cspfj.constraint.extension.Matrix
import cspfj.constraint.extension.Matrix2D
import cspfj.constraint.extension.MatrixGeneral
import cspfj.constraint.extension.TupleTrieSet
import cspfj.Domain
import cspfj.Problem
import cspfj.Variable
import cspom.extension.Trie
import scala.collection.mutable.WeakHashMap
import scala.collection.mutable.HashMap

final class ExtensionGenerator(problem: Problem) extends AbstractGenerator(problem) {

  private case class Signature(domains: Seq[List[Int]], init: Boolean)

  private var generated: List[(Trie, HashMap[Signature, Matrix])] = Nil

  private def generateMatrix(variables: Seq[Variable], relation: Trie, init: Boolean) = {
    val domains = variables map (_.dom)

    val map = generated.find(_._1 eq relation) match {
      case Some(map) => map._2
      case None => {
       val map = new HashMap[Signature, Matrix]
       generated ::= (relation, map)
       map
      }
    }

    val signature = Signature(domains map (_.values.toList), init)

    map.getOrElseUpdate(signature, {
      //println("Generating " + relation + " for " + signature + " not found in " + map)
      gen(relation, init, domains)
    })
  }

  private def gen(relation: Trie, init: Boolean, domains: Seq[Domain]) = {
    val matrix = ExtensionGenerator.bestMatrix(relation, init, domains map (_.size))
    ExtensionGenerator.fillMatrix(domains, relation, init, matrix)
    matrix
  }

  override def generateExtension(extensionConstraint: cspom.extension.ExtensionConstraint) = {

    val solverVariables = extensionConstraint.scope map cspom2cspfj

    if (solverVariables exists (_.dom == null)) {
      false
    } else {

      val matrix = generateMatrix(solverVariables, extensionConstraint.relation, extensionConstraint.init);

      addConstraint(ExtensionConstraint.newExtensionConstraint(matrix, solverVariables.toArray));
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

  def bestMatrix(relation: Trie, init: Boolean, sizes: Seq[Int]) = {
    if (relation.arity == 2) {
      new Matrix2D(sizes(0), sizes(1), init);
    } else if (!init && tupleSetBetterThanMatrix(sizes, relation.size)) {
      new TupleTrieSet(relation.arity, init);
    } else {
      new MatrixGeneral(sizes.toArray, init);
    }
  }

  def fillMatrix(domains: Seq[Domain], relation: Trie, init: Boolean, matrix: Matrix) {

    for (values <- relation.iterator) {
      val tuple = (values, domains).zipped.map { (v, d) => d.index(v) }
      matrix.set(tuple, !init)
    }

  }
}
