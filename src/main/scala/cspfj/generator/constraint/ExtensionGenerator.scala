package cspfj.generator.constraint;

import scala.collection.mutable.HashMap
import cspfj.Domain
import cspfj.Parameter
import cspfj.ParameterManager
import cspfj.Problem
import cspfj.UNSATException
import cspfj.Variable
import cspfj.constraint.extension.ArrayTrie
import cspfj.constraint.extension.ExtensionConstraint2D
import cspfj.constraint.extension.ExtensionConstraintFind
import cspfj.constraint.extension.ExtensionConstraintGeneral
import cspfj.constraint.extension.ExtensionConstraintReduceable
import cspfj.constraint.extension.MDD
import cspfj.constraint.extension.Matrix
import cspfj.constraint.extension.Matrix2D
import cspfj.constraint.extension.STR
import cspfj.constraint.extension.TupleTrieSet
import cspom.extension.HashTrie
import cspfj.constraint.extension.MDD2

object ExtensionGenerator {

  @Parameter("relationAlgorithm")
  var consType = "Reduce"

  @Parameter("relationStructure")
  var ds = "MDD"

  ParameterManager.register(this)

  val TIGHTNESS_LIMIT = 4;

  def bestMatrix(relation: HashTrie, init: Boolean, sizes: Seq[Int]) = {
    if (relation.depth == 2) {
      new Matrix2D(sizes(0), sizes(1), init);
    } else {
      new TupleTrieSet(ds match {
        case "MDD" => new MDD()
        case "MDD2" => new MDD2()
        case "STR" => new STR()
        case "Trie" => ArrayTrie.empty
      }, init)
    }
  }

  def fillMatrix(domains: Seq[Domain], relation: HashTrie, init: Boolean, matrix: Matrix) {

    matrix.setAll(relation.iterator.map {
      values => (values, domains).zipped.map { (v, d) => d.index(v) }
    }.toIterable, !init)

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
      (extensionConstraint.init == true) || (throw new UNSATException)
    } else if (solverVariables exists (_.dom == null)) {
      false
    } else {

      val matrix = generateMatrix(solverVariables, extensionConstraint.relation, extensionConstraint.init);
      val scope = solverVariables.toArray
      val constraint = matrix match {
        case m: Matrix2D => new ExtensionConstraint2D(scope, m, true)
        case m: TupleTrieSet if (m.initialContent == false) => {
          ExtensionGenerator.consType match {
            case "Reduce" => {
              new ExtensionConstraintReduceable(scope, m.reduceable.copy)
            }
            case "Find" => {
              new ExtensionConstraintFind(scope, m, true)
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

