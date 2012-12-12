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

}

final class ExtensionGenerator(problem: Problem) extends AbstractGenerator(problem) {

  private case class Signature(domains: Seq[List[Int]], init: Boolean)

  /**
   * Used to cache value to indices conversion
   */
  private val vToICache = new CacheConverter[cspom.extension.Relation, HashMap[Signature, Matrix]]()

  private def generateMatrix(variables: Seq[Variable], relation: cspom.extension.Relation, init: Boolean) = {
    val domains = variables map (_.dom)

    val map = vToICache.getOrAdd(relation, new HashMap[Signature, Matrix])

    val signature = Signature(domains map (_.values.toList), init)

    map.getOrElseUpdate(signature, {
      //println("Generating " + relation + " for " + signature + " not found in " + map)
      gen(relation, init, domains)
    })
  }

  private def gen(relation: cspom.extension.Relation, init: Boolean, domains: Seq[Domain]) = {
    if (relation.arity == 2) {
      new Matrix2D(domains(0).size, domains(1).size, init).setAll(value2Index(domains, relation), !init)
    } else {
      new TupleTrieSet(ExtensionGenerator.ds match {
        case "MDD" => MDD(value2Index(domains, relation))
        case "MDD2" => MDD2(value2Index(domains, relation))
        case "STR" => new STR() ++ value2Index(domains, relation).toIterable
        case "Trie" => ArrayTrie(value2Index(domains, relation))
      }, init)
    }
  }

  private def value2Index(domains: Seq[Domain], relation: cspom.extension.Relation) = new Traversable[Array[Int]] {
    def foreach[A](f: Array[Int] => A) {
      relation.foreach {
        t => f((t, domains).zipped.map { (v, d) => d.index(v) })
      }
    }
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

