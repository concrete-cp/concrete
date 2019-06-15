package concrete.generator.cspompatterns

import concrete.generator.SumGenerator
import cspom.CSPOM.seq2CSPOMSeq
import cspom.CSPOMConstraint
import cspom.compiler.ACCSE
import cspom.variable._


object ACCSE_Sum extends ACCSE[Int] {

  def functions: Seq[String] = Seq("sum")

  override def constraintToArgs(c: CSPOMConstraint[_]): IndexedSeq[Arg] = {
    val (vars, coefs, _, _) = SumGenerator.readCSPOM(c)
    vars zip coefs
  }

  override def define(subexp: List[Arg], aux: CSPOMVariable[_]): (Arg, CSPOMConstraint[_]) = {
    val definition = CSPOMConstraint("sum")(
      CSPOMConstant(-1) :: subexp.map { case (_, k) => CSPOMConstant(k) },
      aux :: subexp.map { case (x, _) => x },
      CSPOMConstant(0))
      .withParam("mode" -> "eq")

    val arg = (aux, 1)

    (arg, definition)
  }

  override def toString(pairexp: List[Arg], dn: CSPOMExpression[_] => String): String =
    pairexp.map(a => toString(a, dn)).mkString(" + ")

  override def toString(pairexp: Arg, dn: CSPOMExpression[_] => String): String =
    s"${pairexp._2}·${pairexp._1.toString(dn)}"

  def replace(subexp: List[Arg], arg: Arg, constraint: Args): Option[Arg] = {

    // Find the factor btw constraint and subexp

    // First obtain actual coefs
    val actualCoefs: Map[CSPOMExpression[_], Int] = subexp.flatMap { case (x, _) => constraint.get(x).map(x -> _) }.toMap

    // Checks whether all variables from subexp are present. May fail if some variable already have been removed
    // by another subexpression
    if (subexp.lengthCompare(actualCoefs.size) == 0) {

      val (firstX, firstK) = subexp.head

      // Actually compute factor
      val globalK = actualCoefs(firstX) / firstK
      // Check also correction for other coefs. May be wrong if there are several occurrences of the same variable in the scope!
      if (subexp.tail.forall { case (x, k) => k * globalK == actualCoefs(x) }) {
        constraint --= subexp.map { case (x, _) => x }
        val newArg = arg.copy(_2 = globalK)
        constraint += newArg
        Some(newArg)
      } else {
        None
      }
    } else {
      None
    }
  }


  def canonize(args: List[Arg]): List[Arg] = {
    val gcd = java.lang.Math.toIntExact(cspom.util.Math.gcd(
      args.map { case (_, constant) => math.abs(constant).toLong }
    ))

    // Variables are always given in the same order -- avoids using Sets
    assert(args == args.sortBy { case (x, _) => x.hashCode })

    // Canonize so that first non-zero arg is positive
    val a = if (args.find(_._2 != 0).exists(_._2 < 0)) {
      args.map { case (x, k) => (x, -k / gcd) }
    } else {
      args.map { case (x, k) => (x, k / gcd) }
    }

    a
  }

  def intersect(subExp1: List[Arg], subExp2: List[Arg], including: List[Arg]): List[Arg] = {


    val subExp2Map = subExp2.toMap
    assert {
      val subExp1Map = subExp1.toMap
      including.forall { case (x, _) => subExp1Map.contains(x) }
    }
    assert(including.forall { case (x, _) => subExp2Map.contains(x) })

    val (firstX, firstK) = including.head
    val k1 = subExp1.collectFirst { case (x, k) if x == firstX => k }.get / firstK
    val k2 = subExp2Map(firstX) / firstK


    val intersection = for {
      // First compute intersection w.r.t variables
      (x1, s1) <- subExp1
      s2 <- subExp2Map.get(x1)

      // Select variables with same factor
      coef = s1 * k2
      if coef == s2 * k1
    } yield (x1, coef)

    // Canonize
    canonize(intersection)

  }

  override def argsToConstraint(original: CSPOMConstraint[_], args: Args): CSPOMConstraint[_] = {
    val (_, _, constant, mode) = SumGenerator.readCSPOM(original)
    val (vars, coefs) = args.unzip
    CSPOMConstraint(original.result)("sum")(seq2CSPOMSeq(coefs.map(CSPOMConstant(_))), seq2CSPOMSeq(vars), CSPOMConstant(constant)) withParam ("mode" -> mode)
  }


}
