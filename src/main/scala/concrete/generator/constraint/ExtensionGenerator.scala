package concrete.generator.constraint;

import com.typesafe.scalalogging.LazyLogging

import Generator.cspom2concreteVar
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
import concrete.constraint.extension.MDDC
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

class ExtensionGenerator(params: ParameterManager) extends Generator with LazyLogging {

  val consType = params.getOrElse("relationAlgorithm", "Reduce")

  val ds = params.getOrElse("relationStructure", "MDD")

  val closeRelations = params.getOrElse("closeRelations", true)

  val TIGHTNESS_LIMIT = 4;

  private def any2Int(d: Domain, v: Any) = {
    v match {
      case v: Int => v
      case true   => 1
      case false  => 0
    }

  }

  private def any2Int(domains: Seq[Domain], relation: cspom.extension.Relation[_]): Set[Seq[Int]] =
    relation.map { t =>
      (domains, t).zipped.map(any2Int)
    }

  private def cspomMDDtoCspfjMDD[A](
    domains: List[Domain],
    relation: cspom.extension.MDD[A],
    map: collection.mutable.Map[cspom.extension.MDD[A], concrete.constraint.extension.MDD]): MDD = {
    relation match {
      case n if n eq cspom.extension.MDDLeaf => concrete.constraint.extension.MDDLeaf
      case n: cspom.extension.MDDNode[A] => map.getOrElseUpdate(n, {
        val (domain, tail) = (domains.head, domains.tail)
        val trie = n.trie

        trie.toSeq match {
          case Seq() => MDD0
          case Seq((v, t)) =>
            new MDD1(cspomMDDtoCspfjMDD(tail, t, map), any2Int(domain, v))

          case Seq((v1, t1), (v2, t2)) =>
            new MDD2(
              cspomMDDtoCspfjMDD(tail, t1, map), any2Int(domain, v1),
              cspomMDDtoCspfjMDD(tail, t2, map), any2Int(domain, v2))

          case trieSeq =>
            val m = trieSeq.map(l => any2Int(domain, l._1)).max
            val concreteTrie = new Array[concrete.constraint.extension.MDD](m + 1)
            val indices = new Array[Int](trieSeq.size)
            var j = 0
            for ((v, t) <- trieSeq) {
              val i = any2Int(domain, v)
              require(i >= 0, s"Could not find $v in $domain")

              concreteTrie(i) = cspomMDDtoCspfjMDD(tail, t, map)
              indices(j) = i
              j += 1

            }

            new MDDn(concreteTrie, indices, j)
        }
      })
    }
  }

  private case class Signature(domains: Seq[Domain], init: Boolean)

  /**
   * Used to cache value to indices conversion
   */
  private val vToICache = new IdMap[cspom.extension.Relation[_], collection.mutable.Map[Signature, Matrix]]()

  private def generateMatrix(variables: List[Variable], relation: cspom.extension.Relation[_], init: Boolean): Matrix = {
    val domains = variables.map(_.initDomain)

    val map = vToICache.getOrElseUpdate(relation, collection.mutable.Map[Signature, Matrix]())

    val signature = Signature(domains, init)

    map.getOrElseUpdate(signature, {
      logger.debug(s"Generating $relation for $signature ($variables) not found in $map")
      gen(relation, init, domains)
    })

  }

  private def gen(relation: cspom.extension.Relation[_], init: Boolean, domains: List[Domain]) = {
    if (relation.nonEmpty && relation.head.size == 2) {
      val matrix = new Matrix2D(domains(0).span.size, domains(1).span.size,
        domains(0).head, domains(1).head, init)
      matrix.setAll(any2Int(domains, relation), !init)
    } else if (init || relation.head.size == 1) {
      new TupleTrieSet(relation2MDD(relation, domains), init)
    } else {
      new TupleTrieSet(ds match {
        case "MDD" =>
          val mdd = relation2MDD(relation, domains)
          mdd
        case "STR" => new STR() ++ any2Int(domains, relation)
      }, init)
    }
  }

  private def relation2MDD(relation: cspom.extension.Relation[_], domains: List[Domain]): MDDRelation = {
    val mdd = relation match {
      case mdd: cspom.extension.MDD[Any] @unchecked =>
        cspomMDDtoCspfjMDD[Any](
          domains,
          mdd,
          new IdMap())
      case r => MDD(any2Int(domains, r))
    }

    new MDDRelation(mdd)
  }

  override def gen(extensionConstraint: CSPOMConstraint[Boolean])(implicit variables: VarMap): Seq[Constraint] = {

    val solverVariables = extensionConstraint.arguments.map(cspom2concreteVar).toList

    val Some(relation: cspom.extension.Relation[_]) = extensionConstraint.params.get("relation")
    val Some(init: Boolean) = extensionConstraint.params.get("init")

    if (relation.isEmpty) {
      if (init == true) { Seq() } else { throw new UNSATException("Empty relation " + extensionConstraint) }
    } else {
      val scope = solverVariables.toArray

      val matrix = generateMatrix(solverVariables, relation, init)

      val constraint = matrix match {
        case m: Matrix2D => BinaryExt(scope, m, true)
        case m: TupleTrieSet if (scope.size == 1) => {
          new UnaryExt(scope.head, m, true)
        }
        case m: TupleTrieSet if (m.initialContent == false) => {
          consType match {
            case "MDDC" =>
              new MDDC(scope, m.reduceable.asInstanceOf[MDDRelation])

            case "Reduce" =>
              val r: Relation = m.reduceable.copy
              val mdd = r.asInstanceOf[MDDRelation]
              logger.info("MDD stats: " + scope.map(_.initDomain.size).max + " " + scope.length + " " + mdd.edges + " " + mdd.lambda)

              new ReduceableExt(scope, r)

            case "Find" =>
              new FindSupportExt(scope, m, true)

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

