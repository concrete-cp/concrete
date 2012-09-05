package cspfj.generator.constraint;

import scala.Array.canBuildFrom
import scala.collection.mutable.HashMap
import scala.math.BigInt.int2bigInt

import cspfj.constraint.extension.ArrayTrie
import cspfj.constraint.extension.ExtensionConstraint2D
import cspfj.constraint.extension.ExtensionConstraintArray
import cspfj.constraint.extension.ExtensionConstraintArrayTrie
import cspfj.constraint.extension.ExtensionConstraintGeneral
import cspfj.constraint.extension.ExtensionConstraintList
import cspfj.constraint.extension.ExtensionConstraintListTrie
import cspfj.constraint.extension.ExtensionConstraintTrie
import cspfj.constraint.extension.ListTrie
import cspfj.constraint.extension.Matrix
import cspfj.constraint.extension.Matrix2D
import cspfj.constraint.extension.MatrixGeneral
import cspfj.constraint.extension.TupleTrieSet
import cspfj.Domain
import cspfj.Parameter
import cspfj.ParameterManager
import cspfj.Problem
import cspfj.UNSATException
import cspfj.Variable
import cspom.extension.HashTrie

object ExtensionGenerator {

  @Parameter("reduction")
  var ds = "Array"

  ParameterManager.register(this)

  val TIGHTNESS_LIMIT = 4;

  def tupleSetBetterThanMatrix(sizes: Seq[Int], nbTuples: Int) = {
    val size = sizes.foldLeft(BigInt(1))(_ * _)
    size > Int.MaxValue || size > (TIGHTNESS_LIMIT * nbTuples)
  }

  def bestMatrix(relation: HashTrie, init: Boolean, sizes: Seq[Int]) = {
    if (relation.depth == 2) {
      new Matrix2D(sizes(0), sizes(1), init);
    } else if (!init && tupleSetBetterThanMatrix(sizes, relation.size)) {
      new TupleTrieSet(relation, init);
    } else {
      new MatrixGeneral(sizes.toArray, init);
    }
  }

  def fillMatrix(domains: Seq[Domain], relation: HashTrie, init: Boolean, matrix: Matrix) {

    for (values <- relation.iterator) {
      val tuple = (values, domains).zipped.map { (v, d) => d.index(v) }
      matrix.set(tuple, !init)
    }

  }
}

final class ExtensionGenerator(problem: Problem) extends AbstractGenerator(problem) {

  private case class Signature(domains: Seq[List[Int]], init: Boolean)

  /**
   * Used to cache value to indices conversion
   */
  private val vToICache = new CacheConverter[HashTrie, HashMap[Signature, Matrix]]()

  private def generateMatrix(variables: Seq[Variable], relation: HashTrie, init: Boolean) = {
    val domains = variables map (_.dom)

    val map = vToICache.getOrAdd(relation, new HashMap[Signature, Matrix])

    val signature = Signature(domains map (_.values.toList), init)

    map.getOrElseUpdate(signature, {
      //println("Generating " + relation + " for " + signature + " not found in " + map)
      gen(relation, init, domains)
    })
  }

  private def gen(relation: HashTrie, init: Boolean, domains: Seq[Domain]) = {
    val matrix = ExtensionGenerator.bestMatrix(relation, init, domains map (_.size))
    ExtensionGenerator.fillMatrix(domains, relation, init, matrix)
    matrix
  }

  /**
   * Used to cache data structure conversion
   */
  private val dsCache = new CacheConverter[Matrix, List[Array[Int]]]()

  override def generateExtension(extensionConstraint: cspom.extension.ExtensionConstraint) = {

    val solverVariables = extensionConstraint.scope map cspom2cspfj

    if (extensionConstraint.relation.isEmpty) {
      if (extensionConstraint.init == true) true
      else throw new UNSATException
    } else if (solverVariables exists (_.dom == null)) {
      false
    } else {

      val matrix = generateMatrix(solverVariables, extensionConstraint.relation, extensionConstraint.init);
      val scope = solverVariables.toArray
      val constraint = matrix match {
        case m: Matrix2D => new ExtensionConstraint2D(scope, m, true)
        case m: TupleTrieSet => {
          ExtensionGenerator.ds match {
            case "Array" => {
              val struct = dsCache.getOrAdd(m, m.toList)
              // Shallow copy necessary and sufficient
              new ExtensionConstraintArray(scope, struct.toArray)
            }
            case "List" => {
              val struct = dsCache.getOrAdd(m, m.toList)
              new ExtensionConstraintList(scope, struct)
            }
            case "HashTrie" => {
              new ExtensionConstraintTrie(scope, m)
            }
            case "ListTrie" => {
              val list = dsCache.getOrAdd(m, m.toList)
              new ExtensionConstraintListTrie(scope, ListTrie(list: _*))
            }
            case "ArrayTrie" => {
              val list = dsCache.getOrAdd(m, m.toList)
              new ExtensionConstraintArrayTrie(scope, ArrayTrie(list: _*))
            }
            case "General" => {
              new ExtensionConstraintGeneral(m, true, scope)
            }
          }
        }
        case m => new ExtensionConstraintGeneral(m, true, scope)
      }
      addConstraint(constraint)
      true;
    }
  }
}

