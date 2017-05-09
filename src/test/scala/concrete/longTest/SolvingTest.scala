package concrete.longTest;

import scala.collection.mutable.LinkedHashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.Success

import org.scalatest.FunSpec
import org.scalatest.Inspectors
import org.scalatest.Matchers

import concrete.CSPOMSolver
import concrete.ParameterManager
import concrete.Solver
import concrete.generator.ProblemGenerator
import concrete.generator.cspompatterns.Bool2IntIsEq
import concrete.generator.cspompatterns.ConcretePatterns
import concrete.generator.cspompatterns.FZPatterns
import concrete.generator.cspompatterns.XCSPPatterns
import concrete.runner.XCSPConcrete
import cspom.CSPOM
import cspom.compiler.CSPOMCompiler
import cspom.flatzinc.FlatZincFastParser
import cspom.xcsp.XCSPParser
import com.typesafe.scalalogging.LazyLogging

class SolvingTest extends FunSpec with SolvingBehaviors {

  val problemBank = LinkedHashMap[String, (AnyVal, Boolean)](

    "1d_rubiks_cube.small.fzn.xz" -> ((4, false)),
    "1d_rubiks_cube.fzn.xz" -> ((12, false)),
    "testExtension1.xml.xz" -> ((8, false)),
    "testExtension2.xml.xz" -> ((8, false)),
    "testExtension3.xml.xz" -> ((0, false)),

    "testPrimitive.xml.xz" -> ((2, false)),
    "AllInterval-005.xml.xz" -> ((8, false)),
    "testObjective1.xml.xz" -> ((11, false)),
    "QuadraticAssignment-qap.xml.xz" -> ((4776, false)),
    //"fapp01-0200-0.xml.xz" -> ((false, false)),

    "scen11-f12.xml.xz" -> ((0, true)),
    "normalized-renault-mod-0_ext.xml.xz" -> ((true, true)),
    "battleships10.fzn.xz" -> ((1, false)),
    "photo.fzn.xz" -> ((8, false)),
    "crossword-m1-debug-05-01.xml.xz" -> ((48, true)),

    "bigleq-50.xml.xz" -> ((1, true)),
    "battleships_2.fzn.xz" -> ((36, false)),
    "flat30-1.cnf.xz" -> ((true, false)),
    "alpha.fzn.xz" -> ((true, false)),
    "frb35-17-1_ext.xml.xz" -> ((2, true)),

    "queens-12_ext.xml.xz" -> ((14200, false)),
    "zebra.xml.xz" -> ((1, true)),
    "bqwh-18-141-47_glb.xml.xz" -> ((10, true)),
    "crossword-m2-debug-05-01.xml.xz" -> ((48, true)),
    "queens-8.xml.xz" -> ((92, true)),

    "scen11.xml.xz" -> ((true, true)),
    "bqwh-15-106-0_ext.xml.xz" -> ((182, true)),
    "queensAllDiff-8.xml.xz" -> ((92, true)),
    "langford-2-4-ext.xml.xz" -> ((2, true)),
    "series-15.xml.xz" -> ((true, true)),

    "e0ddr1-10-by-5-8.xml.xz" -> ((true, true)),
    "tsp-20-1_ext.xml.xz" -> ((true, true)),
    "test.fzn.xz" -> ((true, false)),
    "queens-12.xml.xz" -> ((14200, false))) .slice(5, 6)

  val parameters = Nil

  for ((p, (r, test)) <- problemBank) {

    val expected: Boolean = r match {
      case e: Int => e > 0
      case b: Boolean => b
      case _ => throw new IllegalArgumentException
    }

    describe(p) {

      it should behave like
        solve(p, expected, parameters, test)

      r match {
        case e: Int if e > 0 =>
          it should behave like
            count(p, e, parameters, test)

        case _ =>
      }

    }
  }

}
trait SolvingBehaviors extends Matchers with Inspectors with LazyLogging { this: FunSpec =>

  def solve(name: String, expectedResult: Boolean, parameters: Seq[(String, String)], test: Boolean): Unit =
    it(s"should have ${if (expectedResult) "a" else "no"} solution") {
      val pm = new ParameterManager
      parameters.foreach(s => pm.update(s._1, s._2))

      val url = getClass.getResource(name)

      require(url != null, "Could not find resource " + name)

      val parser = CSPOM.autoParser(url).get

      CSPOM.load(url, parser)
        .flatMap { cspomProblem =>

          logger.debug(cspomProblem.toString)
          parser match {
            case FlatZincFastParser =>
              CSPOMCompiler.compile(cspomProblem, FZPatterns())

            case XCSPParser =>
              CSPOMCompiler.compile(cspomProblem, XCSPPatterns())

            case _ =>
              Success(cspomProblem)
          }
        }
        .flatMap(CSPOMCompiler.compile(_, ConcretePatterns(pm)))
        .flatMap(CSPOMCompiler.compile(_, Seq(Bool2IntIsEq)))
        .flatMap { cspom =>
          logger.debug(cspom.toString)
          val pg = new ProblemGenerator(pm)

          pg.generate(cspom).flatMap {
            case (problem, variables) =>
              val solver = Solver(problem)
              solver.statistics.register("compiler", CSPOMCompiler)
              solver.statistics.register("generator", pg)
              new CSPOMSolver(solver, cspom, variables).applyGoal()
          }
        }
        .map { solver =>

          //    println(solver.concreteProblem)

          val f = Future {

            val solvIt = solver.toIterable

            if (expectedResult) {
              solvIt should not be 'empty
            } else {
              solvIt shouldBe 'empty
            }

            // println(solver.concreteProblem.toString)

            if (test) {
              for (sol <- solvIt.headOption) {
                val variables: Seq[String] = solver.cspom.goal.get.getSeqParam("variables")
                val failed = XCSPConcrete.controlCSPOM(sol, variables, url)
                withClue(sol) {
                  failed shouldBe 'empty
                }
              }
            }
          }

          concurrent.Await.result(f, 600.seconds)

        }
        .get
    }

  def count(name: String, expectedResult: Int, parameters: Seq[(String, String)],
    test: Boolean): Unit = {
    val url = getClass.getResource(name)

    require(url != null, "Could not find resource " + name)
    val parser = CSPOM.autoParser(url).get

    val pm = new ParameterManager
    parameters.foreach(s => pm.update(s._1, s._2))

    it("should find solutions") {
      CSPOM.load(url, parser)
        .flatMap { cspomProblem =>
          parser match {
            case FlatZincFastParser =>
              CSPOMCompiler.compile(cspomProblem, FZPatterns())

            case XCSPParser =>
              CSPOMCompiler.compile(cspomProblem, XCSPPatterns())

            case _ =>
              Success(cspomProblem)

          }
        }
        .flatMap(
          CSPOMCompiler.compile(_, ConcretePatterns(pm)))
        .flatMap(
          CSPOMCompiler.compile(_, Seq(Bool2IntIsEq)))
        .flatMap { cspom =>
          val pg = new ProblemGenerator(pm)
          pg.generate(cspom).flatMap {
            case (problem, variables) =>
              val solver = Solver(problem)
              solver.statistics.register("compiler", CSPOMCompiler)
              solver.statistics.register("generator", pg)
              new CSPOMSolver(solver, cspom, variables).applyGoal()
          }
        }
        .map { solver =>

          val desc = solver.optimizes match {
            case Some(v) => s"should find optimal value $expectedResult for $v"
            case None => s"should have $expectedResult solutions"
          }

          withClue(desc) {

            val f = Future {
              solver.optimizes match {
                case Some(v) =>
                  val opt = solver.toIterable.last
                  opt.get(v).get shouldBe expectedResult

                case None =>
                  val sols = solver.toStream.take(expectedResult + 1)

                  if (test) {
                    forAll(sols) { sol =>
                      val variables: Seq[String] =
                        solver.cspom.goal.get.getSeqParam("variables")
                      val failed = XCSPConcrete.controlCSPOM(sol, variables, url)
                      failed shouldBe 'empty
                    }
                  }

                  sols should have size expectedResult

              }

            }

            concurrent.Await.result(f, 1000.seconds)
          }
        }
        .get

    }
  }
}
