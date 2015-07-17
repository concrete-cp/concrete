package concrete.longTest;

import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import org.scalatest.Matchers
import com.typesafe.scalalogging.LazyLogging
import concrete.ParameterManager
import concrete.SlowTest
import concrete.SolverFactory
import concrete.heuristic.revision.DomCtr
import concrete.runner.XCSPConcrete
import cspom.CSPOM
import scala.collection.mutable.LinkedHashMap
import scala.util.Failure
import org.scalatest.TryValues
import concrete.Solver
import concrete.generator.cspompatterns.FZPatterns
import concrete.generator.cspompatterns.XCSPPatterns
import cspom.compiler.CSPOMCompiler
import cspom.xcsp.XCSPParser
import cspom.flatzinc.FlatZincParser
import concrete.runner.FZConcrete
import cspom.flatzinc.FZSolve
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
//import SolvingTest._

class SolvingTest extends FlatSpec with SolvingBehaviors {

  behavior of "default parameters"

  it should behave like test()
}

trait SolvingBehaviors extends Matchers with Inspectors with LazyLogging { this: FlatSpec =>

  val problemBank = LinkedHashMap[String, AnyVal](
    "1d_rubiks_cube.fzn" -> 12,
    "scen11-f12.xml.bz2" -> 0,
    "crossword-m1-debug-05-01.xml" -> 48,
    "bigleq-50.xml" -> 1,
    "battleships_2.fzn" -> 36,
    "flat30-1.cnf" -> true,
    "alpha.fzn" -> true,
    "frb35-17-1_ext.xml.bz2" -> 2,
    "queens-12_ext.xml" -> 14200,

    "zebra.xml" -> 1,

    "bqwh-18-141-47_glb.xml.bz2" -> 10,

    "crossword-m2-debug-05-01.xml" -> 48,
    "queens-8.xml" -> 92,

    "scen11.xml.bz2" -> true,
    "bqwh-15-106-0_ext.xml" -> 182,
    "queensAllDiff-8.xml.bz2" -> 92,

    "langford-2-4-ext.xml" -> 2,

    "series-15.xml.bz2" -> true,
    "e0ddr1-10-by-5-8.xml.bz2" -> true,

    "tsp-20-1_ext.xml.bz2" -> true,

    "test.fzn" -> true,

    "queens-12.xml" -> 14200)

  def test(parameters: Seq[(String, String)] = Nil): Unit = {
    for ((p, r) <- problemBank) {

      val expected: Boolean = r match {
        case e: Int     => e > 0
        case b: Boolean => b
        case _          => throw new IllegalArgumentException
      }

      it should "solve " + p taggedAs (SlowTest) in {
        solve(p, expected, parameters)
      }
      r match {
        case e: Int if e > 0 => it should s"find $e solutions to $p" taggedAs (SlowTest) in {
          count(p, e, parameters, false)
        }
        case _ =>
      }

    }
  }

  def solve(name: String, expectedResult: Boolean, parameters: Seq[(String, String)]): Unit = {

    val pm = new ParameterManager
    parameters.foreach(s => pm.update(s._1, s._2))

    val url = getClass.getResource(name)

    require(url != null, "Could not find resource " + name)

    val parser = CSPOM.autoParser(url).get

    var goal: Option[FZSolve] = None

    CSPOM.load(url, parser).flatMap {
      case (cspomProblem, data) =>
        val test = parser match {
          case FlatZincParser =>
            CSPOMCompiler.compile(cspomProblem, FZPatterns()).get
            goal = data.get('goal).collect {
              case g: FZSolve =>
                FZConcrete.parseGoal(g, false, pm)
                g
            }
            false
          case XCSPParser =>
            CSPOMCompiler.compile(cspomProblem, XCSPPatterns()).get
            true
          case _ =>
            false
        }

        //logger.info(cspomProblem.toString)

        Solver(cspomProblem, pm).map { solver =>

          for (g <- goal) {
            FZConcrete.parseSearchMode(g, solver, false)
          }

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
                val failed = XCSPConcrete.controlCSPOM(sol, data('variables).asInstanceOf[Seq[String]], url)
                withClue(sol) {
                  failed shouldBe 'empty
                }
              }
            }
          }

          concurrent.Await.result(f, 1000.seconds)
        }
    }
      .get
  }

  def count(name: String, expectedResult: Int, parameters: Seq[(String, String)],
            test: Boolean): Unit = {
    val url = getClass.getResource(name)

    require(url != null, "Could not find resource " + name)
    val parser = CSPOM.autoParser(url).get

    var goal: Option[FZSolve] = None

    val pm = new ParameterManager

    CSPOM.load(url, parser).flatMap {
      case (cspomProblem, data) =>
        val test = parser match {
          case FlatZincParser =>
            CSPOMCompiler.compile(cspomProblem, FZPatterns()).get
            goal = data.get('goal).collect {
              case g: FZSolve =>
                FZConcrete.parseGoal(g, false, pm)
                g
            }
            false
          case XCSPParser =>
            CSPOMCompiler.compile(cspomProblem, XCSPPatterns()).get
            true
          case _ =>
            false
        }

        logger.info(cspomProblem.toString)

        parameters.foreach(s => pm.update(s._1, s._2))

        Solver(cspomProblem, pm).map { solver =>

          for (g <- goal) {
            FZConcrete.parseSearchMode(g, solver, false)
          }

          val f = Future {
            solver.optimizes match {
              case Some(v) =>
                val opt = solver.toIterable.last
                opt.get(v).get shouldBe expectedResult

              case None =>
                val sols = solver.toStream.take(expectedResult + 1)

                if (test) {
                  forAll(sols.take(100)) { sol =>
                    val failed = XCSPConcrete.controlCSPOM(sol, data('variables).asInstanceOf[Seq[String]], url)
                    failed shouldBe 'empty
                  }
                }

                sols should have size expectedResult
            }

          }
          concurrent.Await.result(f, 1000.seconds)
        }

    } should be a 'success

  }
}
