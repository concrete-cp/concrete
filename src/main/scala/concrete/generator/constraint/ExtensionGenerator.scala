package concrete.generator.constraint;

import scala.collection.mutable.HashMap
import concrete.Domain
import concrete.Parameter
import concrete.ParameterManager
import concrete.Problem
import concrete.UNSATException
import concrete.Variable
import concrete.constraint.extension._
import scala.collection.mutable.ArrayBuffer
import concrete.UNSATObject
import cspom.CSPOMConstraint
import cspom.xcsp.Extension
import concrete.constraint.Constraint
import Generator._

object ExtensionGenerator extends Generator {

  @Parameter("relationAlgorithm")
  var consType = "Reduce"

  @Parameter("relationStructure")
  var ds = "MDD"

  @Parameter("closeRelations")
  var closeRelations = true

  ParameterManager.register(this)

  val TIGHTNESS_LIMIT = 4;

  def cspomMDDtoCspfjMDD(
    domains: List[Domain],
    relation: cspom.extension.MDD,
    map: HashMap[cspom.extension.MDD, concrete.constraint.extension.MDD]): MDD = {
    relation match {
      case cspom.extension.MDDLeaf => concrete.constraint.extension.MDDLeaf
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
            val concreteTrie = new Array[concrete.constraint.extension.MDD](m + 1)
            val indices = new Array[Int](s)
            var j = 0
            for ((v, t) <- trie) {
              val i = domain.index(v)
              concreteTrie(i) = cspomMDDtoCspfjMDD(tail, t, map)
              indices(j) = i
              j += 1
            }

            new MDDn(concreteTrie, indices, indices.length)
        }
      })
    }
  }

  private case class Signature(domains: Seq[List[Int]], init: Boolean)

  /**
   * Used to cache value to indices conversion
   */
  private val vToICache = new CacheConverter[cspom.extension.Relation, HashMap[Signature, Matrix]]()

  private def generateMatrix(variables: List[Variable], relation: cspom.extension.Relation, init: Boolean): Matrix = {
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
          case mdd: cspom.extension.MDD =>
            ExtensionGenerator.cspomMDDtoCspfjMDD(domains, mdd, new HashMap())
          case mdd: cspom.extension.LazyMDD =>
            ExtensionGenerator.cspomMDDtoCspfjMDD(domains, mdd.apply, new HashMap())
          case r =>
            val m = MDD(value2Index(domains, r))
            m
        }
        case "STR" => new STR() ++ value2Index(domains, relation).toIterable
      }, init)
    }
  }

  private def value2Index(domains: Seq[Domain], relation: cspom.extension.Relation): Iterator[Array[Int]] =
    relation.iterator.map { t => (t, domains).zipped.map { (v, d) => d.index(v) } }

  /**
   * Used to cache data structure conversion
   */
  private val dsCache = new CacheConverter[Matrix, List[Array[Int]]]()

  override def gen(extensionConstraint: CSPOMConstraint)(implicit variables: VarMap): Option[Seq[Constraint]] = {

    val solverVariables = extensionConstraint.arguments map cspom2concreteVar toList

    val Some(relation: cspom.extension.Relation) = extensionConstraint.params.get("relation")
    val Some(init: Boolean) = extensionConstraint.params.get("init")

    if (relation.isEmpty) {
      if (init == true) { Some(Seq()) } else { throw UNSATObject }
    } else if (solverVariables.exists(_.dom.undefined)) {
      None
    } else {
      val matrix = generateMatrix(solverVariables, relation, init);
      val scope = solverVariables.toArray
      val constraint = matrix match {
        case m: Matrix2D => BinaryExt(scope, m, true)
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
        case m: Matrix => new ExtensionConstraintGeneral(m, true, scope)
      }
      //      if (ExtensionGenerator.closeRelations) {
      //        extensionConstraint.closeRelation()
      //      }
      //println(extensionConstraint + " -> " + constraint);
      Some(Seq(constraint))

    }
  }
}

