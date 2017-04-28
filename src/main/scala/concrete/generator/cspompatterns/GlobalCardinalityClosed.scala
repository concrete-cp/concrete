package concrete
package generator.cspompatterns

import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMSeq
import cspom.variable.IntExpression
import cspom.variable.IntVariable
import cspom.variable.CSPOMConstant

/**
 * Requires that the number of occurences of \p i in \a x is \a counts[\p i].
 *
 * The elements of \a x must take their values from \a cover.
 *
 * predicate global_cardinality_closed(array[int] of var int: x,
 * array[int] of int: cover,
 * array[int] of var int: counts)
 *
 *
 */
object GlobalCardinalityClosed extends ConstraintCompilerNoData {
  // TODO À finir, ou pas.
  override def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    constraint.function == 'global_cardinality_closed
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) = {

    val Seq(CSPOMSeq(x), IntExpression.constSeq(cover), IntExpression.constSeq(counts)) = constraint.arguments

    val dummies = (counts, cover).zipped.flatMap { (count, cover) => Seq.fill(count)(cover) }

    val dummyVars = Seq.fill(x.length) { IntVariable(0 until dummies.length) }

    val alldiff = CSPOMDriver.allDifferent(dummyVars: _*)

    val dummyToX = (x zip dummyVars).map { case (x, d) => CSPOMConstraint(x)('element)(CSPOM.constantSeq(dummies), d) }

    val groups = (0 until counts.sum).groupBy(dummies).map(_._2)

    val symmetry = for (group <- groups; Seq(g1, g2) <- group.sliding(2)) yield {
      val b1 = problem.defineBool(b => CSPOMConstraint(b)('member)(CSPOMSeq(dummyVars: _*), CSPOMConstant(g1)))
      val b2 = problem.defineBool(b => CSPOMConstraint(b)('member)(CSPOMSeq(dummyVars: _*), CSPOMConstant(g2)))
      CSPOMConstraint('clause)(CSPOMSeq(b1), CSPOMSeq(b2))
    }

    replaceCtr(constraint, alldiff +: symmetry ++: dummyToX, problem)
  }
}