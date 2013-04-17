package cspfj.generator.constraint;

import scala.collection.mutable.HashMap
import cspfj.Domain
import cspfj.Parameter
import cspfj.ParameterManager
import cspfj.Problem
import cspfj.UNSATException
import cspfj.Variable
import cspfj.constraint.extension._
import scala.collection.mutable.ArrayBuffer

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

  private def generateMatrix(variables: List[Variable], relation: cspom.extension.Relation, init: Boolean) = {
    val domains = variables map (_.dom)

    val map = vToICache.getOrAdd(relation, new HashMap[Signature, Matrix])

    val signature = Signature(domains map (_.values.toList), init)

    map.getOrElseUpdate(signature, {
      //println("Generating " + relation + " for " + signature + " not found in " + map)
      gen(relation, init, domains)
    })
  }

  private def gen(relation: cspom.extension.Relation, init: Boolean, domains: List[Domain]) = {
    if (relation.arity == 2) {
      new Matrix2D(domains(0).size, domains(1).size, init).setAll(value2Index(domains, relation).toTraversable, !init)
    } else if (init) {
      new TupleTrieSet(MDD(value2Index(domains, relation)), init)
    } else {
      new TupleTrieSet(ExtensionGenerator.ds match {
        case "MDD" => relation match {
          case mdd: cspom.extension.MDD => cspomMDDtoCspfjMDD(domains, mdd, new HashMap())
          case mdd: cspom.extension.LazyMDD => cspomMDDtoCspfjMDD(domains, mdd.apply, new HashMap())
          case r => MDD(value2Index(domains, r))
        }
        case "STR" => new STR() ++ value2Index(domains, relation).toIterable
      }, init)
    }
  }

  private def cspomMDDtoCspfjMDD(
    domains: List[Domain],
    relation: cspom.extension.MDD,
    map: HashMap[cspom.extension.MDD, cspfj.constraint.extension.MDD]): MDD = {
    relation match {
      case cspom.extension.MDDLeaf => cspfj.constraint.extension.MDDLeaf
      case n: cspom.extension.MDDNode => map.getOrElseUpdate(n, {
        val (domain, tail) = (domains.head, domains.tail)
        val trie = n.trie

        trie.size match {
          case 1 =>

            val (v, t) = trie.head
            new MDD1(cspomMDDtoCspfjMDD(tail, t, map), domain.index(v))

          case 2 =>
            val List((v1, t1), (v2, t2)) = trie.toList
            new MDD2(
              cspomMDDtoCspfjMDD(tail, t1, map), domain.index(v1),
              cspomMDDtoCspfjMDD(tail, t2, map), domain.index(v2))

          case s: Int =>
            val m = trie.map(l => domain.index(l._1)).max
            val cspfjTrie = new Array[cspfj.constraint.extension.MDD](m + 1)
            val indices = new Array[Int](s)
            var j = 0
            for ((v, t) <- trie) {
              val i = domain.index(v)
              cspfjTrie(i) = cspomMDDtoCspfjMDD(tail, t, map)
              indices(j) = i
              j += 1
            }

            new MDDn(cspfjTrie, indices, indices.length)
        }
      })
    }
  }

  private def value2Index(domains: Seq[Domain], relation: cspom.extension.Relation): Iterator[Array[Int]] =
    relation.iterator.map { t => (t, domains).zipped.map { (v, d) => d.index(v) } }

  /**
   * Used to cache data structure conversion
   */
  private val dsCache = new CacheConverter[Matrix, List[Array[Int]]]()

  override def generateExtension(extensionConstraint: cspom.extension.ExtensionConstraint) = {

    val solverVariables = extensionConstraint.scope map cspom2cspfj toList

    if (extensionConstraint.relation.isEmpty) {
      (extensionConstraint.init == true) || (throw new UNSATException)
    } else if (solverVariables exists (_.dom == null)) {
      false
    } else {
      val matrix = generateMatrix(solverVariables, extensionConstraint.relation, extensionConstraint.init);
      val scope = solverVariables.toArray
      val constraint = matrix match {
        case m: Matrix2D => new BinaryExt(scope, m, true)
        case m: TupleTrieSet if (m.initialContent == false) => {
          ExtensionGenerator.consType match {
            case "MDDC" => {
              new MDDC(scope, m.reduceable.asInstanceOf[MDD])
            }
            case "MDDC2" => {
              new MDDC2(scope, m.reduceable.asInstanceOf[MDD])
            }
            case "STR3" => {
              new ExtensionConstraintSTR3(scope, m.reduceable.asInstanceOf[STR].array)
            }
            case "Reduce" => {
              new ReduceableExt(scope, m.reduceable.copy)
            }
            case "Find" => {
              new FindSupportExt(scope, m, true)
            }
            case "General" => {
              new ExtensionConstraintGeneral(m, true, scope)
            }
          }
        }
        case m => new ExtensionConstraintGeneral(m, true, scope)
      }
      extensionConstraint.closeRelation()
      //println(extensionConstraint + " -> " + constraint);
      addConstraint(constraint)

      true;
    }
  }
}

