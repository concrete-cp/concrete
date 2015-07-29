package concrete.generator.cspompatterns

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.CSPOMDriver.CSPOMIntExpressionOperations
import concrete.CSPOMDriver.abs
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.CSPOMCompiler
import cspom.variable.IntVariable
import cspom.xcsp.XCSPParser
import concrete.ParameterManager

class AbsDiffCompilerTest extends FlatSpec with Matchers {

  "AbsDiff compiler" should "compile CSPOM DSL" in {

    val cspom = CSPOM { implicit problem =>

      val v0 = IntVariable(1 to 3) as "v0"
      val v1 = IntVariable(2 to 4) as "v1"

      val r = v0 - v1

      val r2 = abs(r) as "r2"

    }

    CSPOMCompiler.compile(cspom, Seq(AbsDiff)).get

    withClue(cspom.toString) {
      cspom.getPostponed shouldBe empty
      cspom.constraints.toSeq should have size 1
      cspom.referencedExpressions should have size 3

      val v0 = cspom.variable("v0").get
      val v1 = cspom.variable("v1").get
      val r2 = cspom.variable("r2").get

      val Seq(CSPOMConstraint(`r2`, 'absdiff, Seq(`v0`, `v1`), _)) = cspom.constraints.toSeq

     // println(cspom)
    }
  }

  it should "compile XCSP" in {

    val miniScen =
      <instance>
        <presentation maxConstraintArity="2" format="XCSP 2.1"/>
        <domains nbDomains="5">
          <domain name="D0" nbValues="32">16 30 44 58 72 86 100 114 128 142 156 254 268 282 296 310 324 338 352 366 380 394 414 428 442 456 470 484 498 512 526 540</domain>
        </domains>
        <variables nbVariables="2">
          <variable name="V0" domain="D0"/>
          <variable name="V1" domain="D0"/>
          <variable name="V78" domain="D0"/>
        </variables>
        <predicates nbPredicates="2">
          <predicate name="P0">
            <parameters>int X0 int X1 int X2</parameters>
            <expression>
              <functional>gt(abs(sub(X0,X1)),X2)</functional>
            </expression>
          </predicate>
          <predicate name="P1">
            <parameters>int X0 int X1 int X2</parameters>
            <expression>
              <functional>eq(abs(sub(X0,X1)),X2)</functional>
            </expression>
          </predicate>
        </predicates>
        <constraints nbConstraints="2">
          <constraint name="C0" arity="2" scope="V0 V1" reference="P1">
            <parameters>V0 V1 238</parameters>
          </constraint>
          <constraint name="C1" arity="2" scope="V0 V78" reference="P0">
            <parameters>V0 V78 8</parameters>
          </constraint>
        </constraints>
      </instance>

    val p4 = for {
      (p1, data) <- XCSPParser(miniScen)
      p2 <- CSPOMCompiler.compile(p1, XCSPPatterns())
      p3 <- CSPOMCompiler.compile(p2, ConcretePatterns(new ParameterManager()) :+ AbsDiff)
    } yield p3

    val problem = p4.get
    withClue(problem) {
      problem.getPostponed shouldBe empty
      problem.constraints.toSeq should have size 2
    }

  }
}
