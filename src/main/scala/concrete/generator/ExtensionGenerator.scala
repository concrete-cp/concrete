package concrete
package generator

import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.Constraint
import concrete.constraint.extension._
import concrete.generator.Generator.cspom2concrete1D
import cspom.{CSPOMConstraint, UNSATException}
import mdd._

import scala.collection.mutable

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
              val r: Relation = m.relation //.copy

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

  private def any2Int(relation: cspom.extension.Relation[_]): Seq[Seq[Int]] =
    relation.map(_.map(util.Math.any2Int)).toSeq

  private[concrete] def generateMatrix(variables: Seq[Variable], relation: cspom.extension.Relation[_], init: Boolean): Matrix = {
    logger.info(s"Generating matrix for $relation, $variables")
    val domains = variables.map(_.initDomain).toList
    if (relation.nonEmpty && relation.arity == 2) {
      val matrix = new Matrix2D(domains(0).span.size, domains(1).span.size,
        domains(0).head, domains(1).head, init)
      matrix.setAll(any2Int(relation), !init)
    } else if (init || relation.arity == 1) {
      new TupleTrieSet(new MDDRelation(relation2MDD(relation)), init)
    } else if (ds == "Matrix") {
      val matrix = new MatrixGeneral(domains.map(_.span.size).toArray, domains.map(_.head).toArray, init)
      matrix.setAll(any2Int(relation), !init)
    } else {
      new TupleTrieSet(generateRelation(domains.length, relation), init)
    }
  }

  private def generateRelation(arity: Int, relation: cspom.extension.Relation[_]): Relation = {
    ds match {
      case "MDD" => new MDDRelation(relation2MDD(relation))
      case "BDD" => new BDDRelation(relation2BDD(relation))
      case "STR" => STR(any2Int(relation).map(l => l.toArray))
      case "HashTable" => HashTable(any2Int(relation).map(l => l.toArray))
      case "IndexedTable" => IndexedTable(any2Int(relation).map(l => l.toArray))
    }
  }

  private def relation2MDD(relation: cspom.extension.Relation[_]): MDD = {
    relation match {
      case mdd: cspom.extension.MDDRelation => mdd.mdd
      case r => MDD.fromSeq(any2Int(r).map(_.toIndexedSeq))
    }
  }

  private def relation2BDD(relation: cspom.extension.Relation[_]): BDD = {
    BDD(relation2MDD(relation)).reduce()
  }
}

