package concrete.generator.cspompatterns

import concrete.constraint.{Constraint, ConstraintComparator, Residues, TupleEnumerator}
import concrete.generator.ProblemGenerator
import concrete.{IntDomain, Variable}
import cspom.CSPOM._
import cspom.compiler.CSPOMCompiler
import cspom.variable.{CSPOMConstant, CSPOMVariable, IntVariable}
import cspom.{CSPOM, CSPOMConstraint, UNSATException}
import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers, TryValues}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.{Failure, Success}

class PowTest extends FlatSpec with Matchers with ScalaCheckPropertyChecks with TryValues {
  "Pow generator" should "generate correct constraint" in {
    val dom = Gen.nonEmptyListOf(Gen.choose(-100, 100))

    forAll(dom, dom, dom) { (x: Seq[Int], y: Seq[Int], z: Seq[Int]) =>
      test(x, y, z)
    }

  }

  it should "generate correct test case" in {
    test(Seq(36, 20, 55, -2, 100), Seq(2), Seq(4))
  }

  private def test(dx: Seq[Int], dy: Seq[Int], dz: Seq[Int]): Unit = {
    val cspom = CSPOM { implicit problem =>
      val vx = IntVariable(dx: _*) as "x"
      val vy = IntVariable(dy: _*) as "y"
      val vz = IntVariable(dz: _*) as "z"

      ctr(CSPOMConstraint(vz)("pow")(vx, vy))
    }

    CSPOMCompiler.compile(cspom, Seq(Pow))
      .flatMap(cspom => new ProblemGenerator().generate(cspom))
    match {
      case Success((problem, variables)) =>

        val Array(c) = problem.constraints
//
//        println(cspom)
//        println(problem)

        val vx = asVariable("x", cspom, variables)
        val vy = asVariable("y", cspom, variables)
        val vz = asVariable("z", cspom, variables)

        ConstraintComparator.compare(
          Array(vx, vy, vz),
          c,
          referenceConstraint(vx, vy, vz)
        )

      case Failure(_: UNSATException) =>

        val vx = new Variable("x", IntDomain.ofSeq(dx: _*))
        val vy = new Variable("y", IntDomain.ofSeq(dy: _*))
        val vz = new Variable(name = "z", IntDomain.ofSeq(dz: _*))

        ConstraintComparator.checkContradiction(referenceConstraint(vx, vy, vz))

    }


  }

  private def asVariable(name: String, cspom: CSPOM, map: Map[CSPOMVariable[_], Variable]) = {
    cspom.expression(name).get match {
      case v: CSPOMVariable[_] => map(v)
      case CSPOMConstant(const: Int) => new Variable(name, IntDomain.ofSeq(const))
    }
  }

  private def referenceConstraint(vars: Variable*) = {
    new Constraint(vars.toArray) with Residues with TupleEnumerator {
      def check(t: Array[Int]): Boolean = t(1) >= 0 && BigInt(t(0)).pow(t(1)) == t(2)
    }
  }

}
