package concrete
package generator

import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.Constraint
import concrete.constraint.extension._
import concrete.generator.Generator.cspom2concrete1D
import cspom.{CSPOMConstraint, UNSATException}
import mdd._

class ExtensionGenerator(pg: ProblemGenerator) extends Generator with LazyLogging {

  private val algorithm = params.getOrElse("relationAlgorithm", "BDDC")
  private val structure = params.getOrElse("relationStructure", "BDD")
  private val relationCache = new IdMap[MDD, Relation]()

  def params: ParameterManager = pg.pm

  override def gen(extensionConstraint: CSPOMConstraint[Boolean])(implicit variables: VarMap): Seq[Constraint] = {

    val solverVariables = extensionConstraint.arguments.map(cspom2concrete1D).toList

    val Some(mdd) = extensionConstraint.params.get("relation")
      .map {
        case r: cspom.extension.MDDRelation => r.mdd
        case _ => throw new IllegalStateException()
      }

    val Some(init: Boolean) = extensionConstraint.params.get("init")

    if (mdd.isEmpty) {
      if (init) {
        Seq()
      } else {
        throw new UNSATException("Empty relation " + extensionConstraint)
      }
    } else {
      val scope = solverVariables.map(_.asVariable(pg)).toArray
      val domains = scope.map(_.initDomain).toList
      val density = domains.foldLeft(mdd.lambda().toDouble)(_ / _.span.size)

      logger.info(s"Generating extension constraint on ${solverVariables}, density = ${density}")

      val constraint = scope.length match {
        case 1 =>
          // Unary constraint
          new UnaryExt(scope.head, new MDDMatrix(mdd, init))

        case 2 if density > 0.01 =>
          // Binary case
          val matrix = new Matrix2D(domains(0).span.size, domains(1).span.size,
            domains(0).head, domains(1).head, init)
          matrix.setAll(mdd.view.map(a => a.toArray), !init)
          BinaryExt(scope, matrix)

        case _ if !init && algorithm != "General" =>
          // Positive table constraint
          val relation = generateRelation(mdd)
          generateConstraint(scope, relation)

        case _ =>
          new ExtensionConstraintGeneral(new MDDMatrix(mdd, init), true, scope)


      }



      //println(extensionConstraint + " -> " + constraint);
      Seq(constraint)

    }
  }

  private def generateConstraint(scope: Array[Variable], relation: Relation): Constraint = {
    logger.debug(s"Relation stats: ${scope.map(_.initDomain.size).sum} ${scope.length} ${relation.edges} ${relation.lambda} ${relation.depth}")
    algorithm match {
      case "MDDC" =>
        new MDDC(scope, relation.asInstanceOf[MDDRelation])

      case "BDDC" =>
        new BDDC(scope, relation.asInstanceOf[BDDRelation])

      case "Reduce" =>
        new ReduceableExt(scope, relation)

      case "Find" =>
        new FindSupportExt(scope, relation)
    }
  }

  private def generateRelation(relation: MDD): Relation = {
    structure match {
      case "MDD" => relationCache.getOrElseUpdate(relation, new MDDRelation(relation))
      case "BDD" => relationCache.getOrElseUpdate(relation, new BDDRelation(BDD(relation).reduce()))
      case "STR" => relationCache.getOrElseUpdate(relation, STR(relation.toArrayArray)).asInstanceOf[STR].copy
      case "HashTable" => HashTable(relation.toArrayArray)
    }
  }

}

