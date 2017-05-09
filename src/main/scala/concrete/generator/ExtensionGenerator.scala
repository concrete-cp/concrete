package concrete
package generator

import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.Constraint
import concrete.constraint.extension._
import concrete.generator.Generator.cspom2concrete1D
import cspom.extension.IdMap
import cspom.{CSPOMConstraint, UNSATException}

class ExtensionGenerator(pg: ProblemGenerator) extends Generator with LazyLogging {

  private val consType = params.getOrElse("relationAlgorithm", "Reduce")
  private val ds = params.getOrElse("relationStructure", "BDD")

  def params: ParameterManager = pg.pm

  override def gen(extensionConstraint: CSPOMConstraint[Boolean])(implicit variables: VarMap): Seq[Constraint] = {

    val solverVariables = extensionConstraint.arguments.map(cspom2concrete1D(_)).toList

    val Some(relation: cspom.extension.Relation[_]) = extensionConstraint.params.get("relation")
    val Some(init: Boolean) = extensionConstraint.params.get("init")

    if (relation.isEmpty) {
      if (init) {
        Seq()
      } else {
        throw new UNSATException("Empty relation " + extensionConstraint)
      }
    } else {
      val scope = solverVariables.map(_.asVariable(pg)).toArray

      val matrix = generateMatrix(scope, relation, init)

      val constraint = matrix match {
        case m: Matrix2D => BinaryExt(scope, m)
        case m: TupleTrieSet if scope.length == 1 => new UnaryExt(scope.head, m)

        case m: TupleTrieSet if m.initialContent == false => {
          consType match {
            case "MDDC" =>
              new MDDC(scope, m.relation.asInstanceOf[MDDRelation])

            case "BDDC" =>
              new BDDC(scope, m.relation.asInstanceOf[BDDRelation])

            case "Reduce" =>
              val r: Relation = m.relation.copy

              logger.info(s"Relation stats: ${scope.map(_.initDomain.size).sum} ${scope.length} ${r.edges} ${r.lambda} ${r.depth}")

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

  private def any2Int(v: Any) = {
    v match {
      case v: Int => v
      case v: Long if v.isValidInt => v.toInt
      case true => 1
      case false => 0
      case v: Any => throw new AssertionError(s"value $v cannot be handled")
    }

  }

  private def any2Int(relation: cspom.extension.Relation[_]): Iterable[Seq[Int]] =
    relation.map(_.map(any2Int))

  private[concrete] def cspomMDDtoCspfjMDD(
                                            relation: cspom.extension.MDD[Int],
                                            map: collection.mutable.Map[cspom.extension.MDD[Int], concrete.constraint.extension.MDD] = new IdMap()): MDD = {
    relation match {
      case n: Any if n eq cspom.extension.MDDLeaf => concrete.constraint.extension.MDDLeaf
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

          case trieSeq: Any =>
            val m = trieSeq.map(l => any2Int(l._1)).max
            val concreteTrie = new Array[concrete.constraint.extension.MDD](m + 1)

            for ((v, t) <- trieSeq) {
              val i = any2Int(v)
              concreteTrie(i) = cspomMDDtoCspfjMDD(t, map)
            }

            new MDDn(concreteTrie)
        }
      })
    }
  }

  private[concrete] def cspomMDDtoBDD(
                                       relation: cspom.extension.MDD[_],
                                       map: IdMap[cspom.extension.MDD[_], concrete.constraint.extension.BDD] = new IdMap()): BDD = {
    relation match {
      case n: Any if n eq cspom.extension.MDDLeaf => concrete.constraint.extension.BDDLeaf
      case n: cspom.extension.MDDNode[_] => map.getOrElseUpdate(n, {
        n.trie.toSeq
          .map {
            case (k, v) => any2Int(k) -> v
          }
          .sortBy(-_._1)
          .foldLeft[BDD](BDD0) {
          case (acc, (v, st)) =>
            new BDDNode(v,
              cspomMDDtoBDD(st, map),
              acc)
        }

      })
    }
  }

  private[concrete] def generateMatrix(variables: Seq[Variable], relation: cspom.extension.Relation[_], init: Boolean): Matrix = {
    logger.info(s"Generating matrix for $relation, $variables")
    val domains = variables.map(_.initDomain).toList
    if (relation.nonEmpty && relation.arity == 2) {
      val matrix = new Matrix2D(domains(0).span.size, domains(1).span.size,
        domains(0).head, domains(1).head, init)
      matrix.setAll(any2Int(relation), !init)
    } else if (init || relation.arity == 1) {
      new TupleTrieSet(relation2MDD(relation), init)
    } else if (ds == "Matrix") {
      val matrix = new MatrixGeneral(domains.map(_.span.size).toArray, domains.map(_.head).toArray, init)
      matrix.setAll(any2Int(relation), !init)
    } else {
      new TupleTrieSet(generateRelation(domains.length, relation), init)
    }
  }

  private def generateRelation(arity: Int, relation: cspom.extension.Relation[_]): Relation = {
    ds match {
      case "MDD" => relation2MDD(relation)
      case "BDD" => relation2BDD(relation)
      case "STR" => new STR(arity) ++ any2Int(relation)
      case "HashTable" => HashTable(any2Int(relation).toSeq)
      case "IndexedTable" => IndexedTable(any2Int(relation).toSeq)
    }
  }

  private def relation2MDD(relation: cspom.extension.Relation[_]): MDDRelation = {
    val mdd = relation match {
      case mdd: cspom.extension.MDD[Int]@unchecked =>
        cspomMDDtoCspfjMDD(mdd)
      case r => MDD(any2Int(r))
    }

    new MDDRelation(mdd.reduce())
  }

  private def relation2BDD(relation: cspom.extension.Relation[_]): BDDRelation = {
    val mdd = relation match {
      case mdd: cspom.extension.MDD[Int]@unchecked =>
        cspomMDDtoBDD(mdd)
      case r => BDD(any2Int(r).map(_.toList))
    }

    new BDDRelation(bdd = mdd.reduce())
  }
}

