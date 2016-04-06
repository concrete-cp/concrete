package concrete.generator;

import com.typesafe.scalalogging.LazyLogging
import Generator.cspom2concrete1D
import concrete.Domain
import concrete.ParameterManager
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.extension.BinaryExt
import concrete.constraint.extension.ExtensionConstraintGeneral
import concrete.constraint.extension.FindSupportExt
import concrete.constraint.extension.MDD
import concrete.constraint.extension.MDD0
import concrete.constraint.extension.MDD1
import concrete.constraint.extension.MDD2
import concrete.constraint.extension.MDDCd
import concrete.constraint.extension.MDDRelation
import concrete.constraint.extension.MDDn
import concrete.constraint.extension.Matrix
import concrete.constraint.extension.Matrix2D
import concrete.constraint.extension.ReduceableExt
import concrete.constraint.extension.Relation
import concrete.constraint.extension.STR
import concrete.constraint.extension.TupleTrieSet
import concrete.constraint.extension.UnaryExt
import cspom.CSPOMConstraint
import cspom.UNSATException
import cspom.extension.IdMap
import concrete.constraint.extension.HashTable
import concrete.constraint.extension.IndexedTable
import concrete.constraint.extension.MDDLink
import concrete.constraint.extension.MDDLink0
import concrete.constraint.extension.MDDLinkNode
import concrete.constraint.extension.BDDRelation
import scala.collection.mutable.HashMap
import concrete.constraint.extension.MDDCLink
import concrete.util.SparseSeq

class ExtensionGenerator(params: ParameterManager) extends Generator with LazyLogging {

  val consType = params.getOrElse("relationAlgorithm", "Reduce")

  val ds = params.getOrElse("relationStructure", "MDD")

  val closeRelations = params.getOrElse("closeRelations", true)

  val TIGHTNESS_LIMIT = 4;

  private def any2Int(v: Any) = {
    v match {
      case v: Int => v
      case true   => 1
      case false  => 0
    }

  }

  private def any2Int(relation: cspom.extension.Relation[_]): Set[Seq[Int]] =
    relation.map(_.map(any2Int))

  private[concrete] def cspomMDDtoCspfjMDD(
    relation: cspom.extension.MDD[Int],
    map: collection.mutable.Map[cspom.extension.MDD[Int], concrete.constraint.extension.MDD] = new IdMap()): MDD = {
    relation match {
      case n if n eq cspom.extension.MDDLeaf => concrete.constraint.extension.MDDLeaf
      case n: cspom.extension.MDDNode[Int] => map.getOrElseUpdate(n, {
        val trie = n.trie

        trie.toSeq match {
          case Seq() => MDD0
          case Seq((v, t)) =>
            new MDD1(cspomMDDtoCspfjMDD(t, map), any2Int(v))

          case Seq((v1, t1), (v2, t2)) =>
            new MDD2(
              cspomMDDtoCspfjMDD(t1, map), any2Int(v1),
              cspomMDDtoCspfjMDD(t2, map), any2Int(v2))

          case trieSeq =>
            val m = trieSeq.map(l => any2Int(l._1)).max
            val concreteTrie = new Array[concrete.constraint.extension.MDD](m + 1)
            var indices = new SparseSeq[Int]()

            for ((v, t) <- trieSeq) {
              val i = any2Int(v)
              concreteTrie(i) = cspomMDDtoCspfjMDD(t, map)
              indices += i
            }

            new MDDn(concreteTrie, indices)
        }
      })
    }
  }

  private[concrete] def cspomMDDtoMDDLink(
    relation: cspom.extension.MDD[Int],
    map: IdMap[cspom.extension.MDD[Int], concrete.constraint.extension.MDDLink] = new IdMap()): MDDLink = {
    relation match {
      case n if n eq cspom.extension.MDDLeaf => concrete.constraint.extension.MDDLinkLeaf
      case n: cspom.extension.MDDNode[Int] => map.getOrElseUpdate(n, {
        n.trie.toSeq.sortBy(-_._1).foldLeft[MDDLink](MDDLink0) {
          case (acc, (v, st)) =>
            new MDDLinkNode(any2Int(v),
              cspomMDDtoMDDLink(st, map),
              acc)
        }

      })
    }
  }

  private case class Signature(domains: Seq[Domain], init: Boolean)

  /**
   * Used to cache value to indices conversion
   */
  private val vToICache = new IdMap[cspom.extension.Relation[_], collection.mutable.Map[Signature, Matrix]]()

  private def generateMatrix(variables: Seq[Variable], relation: cspom.extension.Relation[_], init: Boolean): Matrix = {
    val domains = variables.map(_.initDomain).toList

    val map = vToICache.getOrElseUpdate(relation, collection.mutable.Map[Signature, Matrix]())

    val signature = Signature(domains, init)

    map.getOrElseUpdate(signature, {
      logger.debug(s"Generating $relation for $signature ($variables) not found in $map")
      gen(relation, init, domains)
    })

  }

  private def gen(relation: cspom.extension.Relation[_], init: Boolean, domains: List[Domain]): Matrix = {
    if (relation.nonEmpty && relation.arity == 2) {
      val matrix = new Matrix2D(domains(0).span.size, domains(1).span.size,
        domains(0).head, domains(1).head, init)
      matrix.setAll(any2Int(relation), !init)
    } else if (init || relation.arity == 1) {
      new TupleTrieSet(relation2MDD(relation), init)
    } else {
      new TupleTrieSet(
        ds match {
          case "MDD"          => relation2MDD(relation)
          case "MDDLink"      => relation2MDDLink(relation)
          case "STR"          => new STR(domains.length) ++ any2Int(relation)
          case "HashTable"    => HashTable(any2Int(relation).toSeq)
          case "IndexedTable" => IndexedTable(any2Int(relation).toSeq)
        }, init)
    }
  }

  private def relation2MDD(relation: cspom.extension.Relation[_]): MDDRelation = {
    val mdd = relation match {
      case mdd: cspom.extension.MDD[Int] @unchecked =>
        cspomMDDtoCspfjMDD(mdd)
      case r => MDD(any2Int(r))
    }

    new MDDRelation(mdd.reduce())
  }

  private def relation2MDDLink(relation: cspom.extension.Relation[_]): BDDRelation = {
    val mdd = relation match {
      case mdd: cspom.extension.MDD[Int] @unchecked =>
        cspomMDDtoMDDLink(mdd)
      case r => MDDLink(any2Int(r).map(_.toList))
    }

    new BDDRelation(bdd = mdd.reduce(), cache = null)
  }

  override def gen(extensionConstraint: CSPOMConstraint[Boolean])(implicit variables: VarMap): Seq[Constraint] = {

    val solverVariables = extensionConstraint.arguments.map(cspom2concrete1D).toList

    val Some(relation: cspom.extension.Relation[_]) = extensionConstraint.params.get("relation")
    val Some(init: Boolean) = extensionConstraint.params.get("init")

    if (relation.isEmpty) {
      if (init == true) { Seq() } else { throw new UNSATException("Empty relation " + extensionConstraint) }
    } else {
      val scope = solverVariables.map(_.asVariable).toArray

      val matrix = generateMatrix(scope, relation, init)

      val constraint = matrix match {
        case m: Matrix2D => BinaryExt(scope, m)
        case m: TupleTrieSet if (scope.size == 1) => {
          new UnaryExt(scope.head, m)
        }
        case m: TupleTrieSet if (m.initialContent == false) => {
          consType match {
            case "MDDCd" =>
              new MDDCd(scope, m.relation.asInstanceOf[MDDRelation])

            case "MDDCnu" =>
              new MDDCLink(scope, m.relation.asInstanceOf[BDDRelation])

            case "Reduce" =>
              val r: Relation = m.relation.copy

              logger.info(s"Relation stats: ${scope.map(_.initDomain.size).max} ${scope.length} ${r.edges} ${r.lambda}")

              new ReduceableExt(scope, r)

            case "Find" =>
              new FindSupportExt(scope, m.relation)

            case "General" =>
              new ExtensionConstraintGeneral(m, true, scope)
          }
        }
        case m: Matrix => new ExtensionConstraintGeneral(m, true, scope)
      }

      //println(extensionConstraint + " -> " + constraint);
      Seq(constraint)

    }
  }
}

