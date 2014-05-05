package concrete.generator.constraint;

import scala.collection.mutable.HashMap
import scala.reflect.runtime.universe
import com.typesafe.scalalogging.slf4j.LazyLogging
import Generator.cspom2concreteVar
import concrete.Domain
import concrete.ParameterManager
import concrete.UNSATObject
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
import concrete.constraint.extension.MDDC2
import concrete.constraint.extension.MDDn
import concrete.constraint.extension.Matrix
import concrete.constraint.extension.Matrix2D
import concrete.constraint.extension.ReduceableExt
import concrete.constraint.extension.STR
import concrete.constraint.extension.TupleTrieSet
import cspom.CSPOMConstraint
import cspom.extension.IdMap

class ExtensionGenerator(params: ParameterManager) extends Generator with LazyLogging {

  val consType = params.getOrElse("relationAlgorithm", "Reduce")

  val ds = params.getOrElse("relationStructure", "MDD")

  val closeRelations = params.getOrElse("closeRelations", true)

  val TIGHTNESS_LIMIT = 4;

  private def cspomMDDtoCspfjMDD(
    domains: List[Domain],
    relation: cspom.extension.MDD[Int],
    map: collection.mutable.Map[cspom.extension.MDD[Int], concrete.constraint.extension.MDD]): MDD = {
    relation match {
      case n if n eq cspom.extension.MDDLeaf => concrete.constraint.extension.MDDLeaf
      case n: cspom.extension.MDDNode[Int] => map.getOrElseUpdate(n, {
        val (domain, tail) = (domains.head, domains.tail)
        val trie = n.trie

        trie.toSeq match {
          case Seq() => MDD0
          case Seq((v, t)) =>
            new MDD1(cspomMDDtoCspfjMDD(tail, t, map), domain.index(v))

          case Seq((v1, t1), (v2, t2)) =>
            new MDD2(
              cspomMDDtoCspfjMDD(tail, t1, map), domain.index(v1),
              cspomMDDtoCspfjMDD(tail, t2, map), domain.index(v2))

          case trieSeq =>
            val m = trieSeq.map(l => domain.index(l._1)).max
            val concreteTrie = new Array[concrete.constraint.extension.MDD](m + 1)
            val indices = new Array[Int](trieSeq.size)
            var j = 0
            for ((v, t) <- trieSeq) {
              val i = domain.index(v)
              if (i < 0) {
                logger.warn(s"Could not find $v in $domain")
              } else {
                concreteTrie(i) = cspomMDDtoCspfjMDD(tail, t, map)
                indices(j) = i
                j += 1
              }
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
  private val vToICache = new CacheConverter[cspom.extension.Relation[_], HashMap[Signature, Matrix]]()

  private def generateMatrix(variables: List[Variable], relation: cspom.extension.Relation[_], init: Boolean): Matrix = {
    val domains = variables map (_.dom)

    val map = vToICache.getOrAdd(relation, new HashMap[Signature, Matrix])

    val signature = Signature(domains map (_.values.toList), init)

    map.getOrElseUpdate(signature, {
      //println("Generating " + relation + " for " + signature + " not found in " + map)
      gen(relation, init, domains)
    })
  }

  private def gen(relation: cspom.extension.Relation[_], init: Boolean, domains: List[Domain]) = {
    if (relation.nonEmpty && relation.head.size == 2) {
      new Matrix2D(domains(0).size, domains(1).size, init).setAll(value2Index(domains, relation).toTraversable.map(_.toArray), !init)
    } else if (init) {
      new TupleTrieSet(MDD(value2Index(domains, relation).map(_.toArray)), init)
    } else {
      new TupleTrieSet(ds match {
        case "MDD" => relation2MDD(relation, domains)
        case "STR" => new STR() ++ value2Index(domains, relation).toIterable.map(_.toArray)
      }, init)
    }
  }

  private def relation2MDD(relation: cspom.extension.Relation[_], domains: List[Domain]): MDD = {
    relation match {
      case mdd: cspom.extension.MDD[_] =>
        cspomMDDtoCspfjMDD(
          domains,
          mdd.asInstanceOf[cspom.extension.MDD[Int]],
          new IdMap())
      case r =>
        val m = MDD(value2Index(domains, r).map(_.toArray))
        m
    }
  }

  private def value2Index(domains: Seq[Domain], relation: cspom.extension.Relation[_]): Iterator[Seq[Int]] =
    relation.iterator.map { t =>
      (t, domains).zipped.map { (v, d) =>
        val i = d.index(v.asInstanceOf[Int])
        if (i < 0) {
          logger.warn(s"Could not find $v in $d")
        }
        i
      }
    } filterNot {
      _.contains(-1)
    }

  /**
   * Used to cache data structure conversion
   */
  private val dsCache = new CacheConverter[Matrix, List[Array[Int]]]()

  override def gen(extensionConstraint: CSPOMConstraint[Boolean])(implicit variables: VarMap): Option[Seq[Constraint]] = {

    val solverVariables = extensionConstraint.arguments map cspom2concreteVar toList

    val Some(relation: cspom.extension.Relation[_]) = extensionConstraint.params.get("relation")
    val Some(init: Boolean) = extensionConstraint.params.get("init")

    if (relation.isEmpty) {
      if (init == true) { Some(Seq()) } else { throw UNSATObject }
    } else if (solverVariables.exists(_.dom.undefined)) {
      None
    } else {
      val matrix = generateMatrix(solverVariables, relation, init)
      val scope = solverVariables.toArray
      val constraint = matrix match {
        case m: Matrix2D => BinaryExt(scope, m, true)
        case m: TupleTrieSet if (m.initialContent == false) => {
          consType match {
            case "MDDC" =>
              new MDDC(scope, m.reduceable.asInstanceOf[MDD])

            case "MDDC2" =>
              new MDDC2(scope, m.reduceable.asInstanceOf[MDD])

            case "Reduce" =>
              val r = m.reduceable.copy
              new ReduceableExt(scope, r)

            case "Find" =>
              new FindSupportExt(scope, m, true)

            case "General" =>
              new ExtensionConstraintGeneral(m, true, scope)
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

