package concrete
package generator

import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.Constraint
import concrete.constraint.extension._
import concrete.generator.Generator.cspom2concrete1D
import cspom.{CSPOMConstraint, UNSATException}
import mdd._

class ExtensionGenerator(pg: ProblemGenerator) extends Generator with LazyLogging {

  private val consType = params.getOrElse("relationAlgorithm", "BDDC")
  private val ds = params.getOrElse("relationStructure", "BDD")
  private val relationCache = new IdMap[MDD, Relation]()

  def params: ParameterManager = pg.pm

  override def gen(extensionConstraint: CSPOMConstraint[Boolean])(implicit variables: VarMap): Seq[Constraint] = {

    val solverVariables = extensionConstraint.arguments.map(cspom2concrete1D(_)).toList

    val Some(relation: cspom.extension.MDDRelation) = extensionConstraint.params.get("relation")
    val Some(init: Boolean) = extensionConstraint.params.get("init")

    if (relation.isEmpty) {
      if (init) {
        Seq()
      } else {
        throw new UNSATException("Empty relation " + extensionConstraint)
      }
    } else {
      val scope = solverVariables.map(_.asVariable(pg)).toArray

      val matrix = generateMatrix(scope, relation.mdd, init)

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

  private[concrete] def generateMatrix(variables: Seq[Variable], relation: MDD, init: Boolean): Matrix = {
    logger.info(s"Generating matrix for $relation, $variables")
    val domains = variables.map(_.initDomain).toList

    val density = domains.foldLeft(relation.lambda().toDouble)(_ / _.span.size)

    relation.depth().collect {
      case 2 if density > .01 =>
        val matrix = new Matrix2D(domains(0).span.size, domains(1).span.size,
          domains(0).head, domains(1).head, init)
        matrix.setAll(relation, !init)


      case 1 if init =>
        new TupleTrieSet(new MDDRelation(relation), init)
      case _ if ds == "Matrix" =>
        val matrix = new MatrixGeneral(domains.map(_.span.size).toArray, domains.map(_.head).toArray, init)
        matrix.setAll(relation, !init)
      //      case _ =>
      //        new TupleTrieSet(generateRelation(domains.length, relation), init)
    }
      .getOrElse {
        new TupleTrieSet(generateRelation(relation), init)
      }
  }

  private def generateRelation(relation: MDD): Relation = {
    ds match {
      case "MDD" => relationCache.getOrElseUpdate(relation, new MDDRelation(relation))
      case "BDD" => relationCache.getOrElseUpdate(relation, new BDDRelation(BDD(relation).reduce()))
      case "STR" => relationCache.getOrElseUpdate(relation, STR(relation.toArrayArray)).asInstanceOf[STR].copy
      case "HashTable" => HashTable(relation.toArrayArray)
      case "IndexedTable" => IndexedTable(relation.toArrayArray)
    }
  }

}

