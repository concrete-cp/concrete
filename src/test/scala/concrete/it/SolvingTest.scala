package concrete
package it


import java.net.URL

import com.typesafe.scalalogging.LazyLogging
import concrete.generator.ProblemGenerator
import concrete.generator.cspompatterns.{ConcretePatterns, FZPatterns, XCSPPatterns}
import concrete.runner.{XCSP3Concrete, XCSP3SolutionChecker}
import cspom.CSPOM.Parser
import cspom.compiler.CSPOMCompiler
import cspom.flatzinc.FlatZincFastParser
import cspom.variable.CSPOMVariable
import cspom.xcsp.XCSP3Parser
import cspom.{CSPOM, UNSATException}
import org.scalatest._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.{Random, Success, Try}


class SolvingTest extends FunSpec with SolvingBehaviors {

  private val lecoutrePB = Seq[(String, (AnyVal, Double))](
    "RadarSurveillance-8-24-3-2-01.xml.xz" -> ((0, 1.0)),
    "qwh-10-57-0_X2.xml.xz" -> ((37, 1.0)),
    "Alpha.xml.xz" -> ((1, 1.0)),
    "Dominoes-grid1.xml.xz" -> ((128, 1.0)),
    "LabeledDice.xml.xz" -> ((48, 1.0)),

    "Zebra.xml.xz" -> ((48, 1.0)),
    "AllInterval-005.xml.xz" -> ((8, 1.0)),
    "Bibd-sc-03-03-01.xml.xz" -> ((1, 1.0)),
    "Bibd-sum-03-03-01.xml.xz" -> ((1, 1.0)),
    "Blackhole-04-3-00.xml.xz" -> ((47232, .05)),

    "Blackhole-4-04-0_X2.xml.xz" -> ((0, 1.0)),
    "CarSequencing-dingbas.xml.xz" -> ((6, 1.0)),
    "ColouredQueens-03.xml.xz" -> ((0, 1.0)),
    "ColouredQueens-05.xml.xz" -> ((240, 1.0)),
    "CostasArray-10.xml.xz" -> ((2160, 1.0)),

    "CryptoPuzzle-send-more-money.xml.xz" -> ((1, 1.0)),
    "DistinctVectors-05-100-02.xml.xz" -> ((true, 1.0)),
    "Domino-100-100.xml.xz" -> ((1, 1.0)),
    "driverlogw-01c.xml.xz" -> ((597126, .005)),
    "Dubois-015.xml.xz" -> ((0, 1.0)),

    "GracefulGraph-K02-P02.xml.xz" -> ((16, 1.0)),
    "Hanoi-04.xml.xz" -> ((2019208, .0001)),
    "Haystacks-04.xml.xz" -> ((0, 1.0)),
    "Kakuro-easy-000-ext.xml.xz" -> ((1, 1.0)),
    "Kakuro-easy-000-sumdiff.xml.xz" -> ((1, 1.0)),

    // 25
    "Knights-008-05.xml.xz" -> ((0, 1.0)),
    "KnightTour-04-ext02.xml.xz" -> ((0, 1.0)),
    "KnightTour-04-int.xml.xz" -> ((0, 1.0)),
    "Langford-2-05.xml.xz" -> ((0, 1.0)),
    "LangfordBin-05.xml.xz" -> ((0, 1.0)),

    "qwh-o005-h10.xml.xz" -> ((1, 1.0)),
    "bqwh-15-106-00_X2.xml.xz" -> ((182, 1.0)),
    "MagicSequence-003-ca.xml.xz" -> ((0, 1.0)),
    "MagicSequence-004-ca.xml.xz" -> ((2, 1.0)),
    "MagicSequence-003-co.xml.xz" -> ((0, 1.0)),

    "MagicSequence-004-co.xml.xz" -> ((2, 1.0)),
    "MagicSquare-3-mdd.xml.xz" -> ((8, 1.0)),
    "MagicSquare-3-sum.xml.xz" -> ((8, 1.0)),
    "MagicSquare-3-table.xml.xz" -> ((8, 1.0)),
    "MultiKnapsack-1-01.xml.xz" -> ((1, 1.0)),

    "MultiKnapsack-1-0_X2.xml.xz" -> ((1, 1.0)),
    "Nonogram-001-regular.xml.xz" -> ((1, 1.0)),
    "Nonogram-001-table.xml.xz" -> ((1, 1.0)),
    "Ortholatin-002.xml.xz" -> ((0, 1.0)),
    "Ortholatin-003.xml.xz" -> ((2, 1.0)),

    "pigeonsPlus-06-03.xml.xz" -> ((0, 1.0)),
    "Primes-10-20-2-1.xml.xz" -> ((113680, .001)),
    "composed-25-01-02-0.xml.xz" -> ((0, 1.0)),
    "ehi-85-297-00.xml.xz" -> ((0, 1.0)),
    "QuasiGroup-3-04.xml.xz" -> ((2, 1.0)),

    // 50
    "QuasiGroup-4-04.xml.xz" -> ((0, 1.0)),
    "QueenAttacking-04_X2.xml.xz" -> ((0, 1.0)),
    "Queens-0012-m1.xml.xz" -> ((14200, .01)),
    "QueensKnights-008-05-add.xml.xz" -> ((0, 1.0)),
    "QueensKnights-008-05-mul.xml.xz" -> ((0, 1.0)),

    "RenaultMod-01.xml.xz" -> ((0, 1.0)),
    "Rlfap-scen-06.xml.xz" -> ((0, 1.0)),
    "RoomMate-magic-10-50-int.xml.xz" -> ((0, 1.0)),
    "RoomMate-sr0006-int.xml.xz" -> ((2, 1.0)),
    "aim-50-1-6-sat-1.xml.xz" -> ((1, 1.0)),

    "Cabinet-5561_X2.xml.xz" -> ((0, 1.0)),
    "SchurrLemma-012-9-mod.xml.xz" -> ((0, 1.0)),
    "SchurrLemma-023-3.xml.xz" -> ((18, 1.0)),
    "SocialGolfers-5-4-3-cp.xml.xz" -> ((true, 1.0)),
    "SportsScheduling-04.xml.xz" -> ((0, 1.0)),

    "SportsScheduling-06.xml.xz" -> ((10, 1.0)),
    "Steiner3-05.xml.xz" -> ((0, 1.0)),
    "Subisomorphism-A-01.xml.xz" -> ((1, 1.0)),
    "Sudoku-s01a-alldiff.xml.xz" -> ((1, 1.0)),
    "Sudoku-s01a-table.xml.xz" -> ((1, 1.0)),

    "SuperQueens-01.xml.xz" -> ((0, 1.0)),
    "TravellingSalesman-25-003_X2.xml.xz" -> ((5, 1.0)),
    "DeBruijnSequence-02-02.xml.xz" -> ((1, 1.0)),
    "DeBruijnSequence-02-04.xml.xz" -> ((16, 1.0)),
    "DiamondFree-004.xml.xz" -> ((0, 1.0)),

    //75
    "DiamondFree-008.xml.xz" -> ((17, 1.0)),
    "MagicHexagon-02-0000.xml.xz" -> ((0, 1.0)),
    "MagicHexagon-03-0001.xml.xz" -> ((1, 1.0)),
    "NumberPartitioning-004.xml.xz" -> ((0, 1.0)),
    "NumberPartitioning-008.xml.xz" -> ((1, 1.0)),

    "PropStress-0020.xml.xz" -> ((0, 1.0)),
    "Wwtpp-jok-ex02000.xml.xz" -> ((0, 1.0)),

    // COP
    "Change-82-100.xml.xz" -> ((5, 1.0)),
    "Recipe.xml.xz" -> ((1700, .1)),
    "BusScheduling-cnt-t1.xml.xz" -> ((7, 1.0)),

    "ChessboardColoration-05-05.xml.xz" -> ((2, 1.0)),
    "ChessboardColoration-03-05.xml.xz" -> ((1, 1.0)),
    "Cutstock-small.xml.xz" -> ((4, 1.0)),
    "Fastfood-ff01.xml.xz" -> ((3050, .1)),
    "GolombRuler-07-a3.xml.xz" -> ((25, 1.0)),

    "GolombRuler-07-a4.xml.xz" -> ((25, 1.0)),
    "GraphColoring-qwhdec-o18-h120-1.xml.xz" -> ((17, 1.0)),
    "GraphColoring-1-fullins-3.xml.xz" -> ((3, 1.0)),
    "Knapsack-30-100-00.xml.xz" -> ((709, 1.0)),
    "LowAutocorrelation-005.xml.xz" -> ((2, 1.0)),

    "Opd-02-035-010.xml.xz" -> ((0, 1.0)),
    "Opd-07-007-003.xml.xz" -> ((1, 1.0)),
    "PrizeCollecting-15-3-5-0.xml.xz" -> ((20, 1.0)),
    "Pb-mps-v2-20-10-bm23.xml.xz" -> ((34, 1.0)),
    "QuadraticAssignment-chr12a.xml.xz" -> ((4776, .1)),

    //100
    "Ramsey-08.xml.xz" -> ((2, 1.0)),
    "Taillard-os-04-04-0.xml.xz" -> ((193, 1.0)),
    "StillLife-03-03.xml.xz" -> ((6, 1.0)),
    "StillLife-wastage-03.xml.xz" -> ((6, 1.0)),
    "Warehouse-opl.xml.xz" -> ((383, 1.0)),

    "ProgressiveParty-rally-red05.xml.xz" -> ((4, 1.0)),
    "OpenStacks-m1-tiny.xml.xz" -> ((3, 1.0)),
    "OpenStacks-m1-pb-10-10-1.xml.xz" -> ((5, 1.0)),
    "OpenStacks-m2-tiny.xml.xz" -> ((3, 1.0)),
    "OpenStacks-m2-pb-10-10-1.xml.xz" -> ((5, 1.0)),

    "Rack-r1.xml.xz" -> ((550, 1.0)),

    // Long
    "Crossword-lex-vg-4-6.xml.xz" -> ((4749, 0.1)),
    "Crossword-lex-vg-4-7.xml.xz" -> ((125, 1.0)),
    "Crossword-lex-vg-4-8.xml.xz" -> ((1, 1.0)),
    "CoveringArray-3-04-2-08.xml.xz" -> ((80640, 1.0)),

    "bdd-15-21-2-2713-79-01.xml.xz" -> ((0, 1.0)),
    "MarketSplit-01.xml.xz" -> ((true, 1.0)),
    "QueenAttacking-05.xml.xz" -> ((0, 1.0)),
    "QueenAttacking-05_X2.xml.xz" -> ((400, 1.0)),
    "Steiner3-07.xml.xz" -> ((151200, .001)),

    //Uses unimplemented circuit constraint
    //"Mario-easy-2.xml.xz" -> ((628, 1.0)),
    //"Tpp-3-3-20-1.xml.xz" -> ((126, 1.0))
  )


  private val problemBank = Seq[(String, (AnyVal, Boolean))](
    //  "celar-CELAR6-SUB2.fzn.xz" -> ((true, false)),
    "1d_rubiks_cube.small.fzn.xz" -> ((4, false)),
    "1d_rubiks_cube.fzn.xz" -> ((12, false)),
    "battleships10.fzn.xz" -> ((1, false)),
    "photo.fzn.xz" -> ((8, false)),
    "bigleq-50.xml.xz" -> ((1, true)),
    "battleships_2.fzn.xz" -> ((36, false)),
    "flat30-1.cnf.xz" -> ((true, false)),
    "alpha.fzn.xz" -> ((true, false)),
    "test.fzn.xz" -> ((true, false)))
    .map { case (pb, (nbsol, test)) => (pb, (nbsol, 0.0)) }

  private val parameters = new ParameterManager() //.updated("f", Unit).updated("heuristic.value", classOf[BestCost])

  for ((p, (r, test)) <-
       lecoutrePB ++
         problemBank
  ) {

    describe(p) {

      it should behave like
        count(p, r, parameters, test)

    }
  }

}

trait SolvingBehaviors extends Matchers with Inspectors with OptionValues with LazyLogging {
  this: FunSpec =>


  val slow = Set(
    "CostasArray-10.xml.xz",
    "driverlogw-01c.xml.xz",
    "Hanoi-04.xml.xz",
    "Queens-0012-m1.xml.xz",
    "OpenStacks-m1-pb-10-10-1.xml.xz",
    "Crossword-lex-vg-4-6.xml.xz",
    "Crossword-lex-vg-4-7.xml.xz",
    "Crossword-lex-vg-4-8.xml.xz",
    "CoveringArray-3-04-2-08.xml.xz",

    "bdd-15-21-2-2713-79-01.xml.xz",
    "MarketSplit-01.xml.xz",
    "QueenAttacking-05.xml.xz",
    "QueenAttacking-05_X2.xml.xz",
    "Steiner3-07.xml.xz",
    "SocialGolfers-5-4-3-cp.xml.xz",
    "PrizeCollecting-15-3-5-0.xml.xz",
    "OpenStacks-m2-pb-10-10-1.xml.xz",
    "Cabinet-5561_X2.xml.xz",
    "Pb-mps-v2-20-10-bm23.xml.xz",
    "1d_rubiks_cube.fzn.xz"
  )

  def count(name: String, expectedResult: AnyVal, pm: ParameterManager,
            test: Double): Unit = {
    val url = getClass.getResource(name)

    require(url != null, "Could not find resource " + name)

    it("should find solutions", Seq(SlowTest).filter(_ => slow(name)): _*) {

      val result: Try[_] = for {
        parser <- Try(CSPOM.autoParser(url).get)
        cspom <- CSPOM.load(url, parser)
          .flatMap(compile(parser, _))
          .flatMap(CSPOMCompiler.compile(_, ConcretePatterns(pm)))
        pg = new ProblemGenerator(pm)
        (problem, variables) <- pg.generate(cspom)
        concrete <- Solver(problem, pm)
      } yield {
        concrete.statistics.register("compiler", CSPOMCompiler)
        concrete.statistics.register("generator", pg)
        val solver = new CSPOMSolver(concrete, cspom.expressionMap, variables)
        val declared = cspom.goal.get.getSeqParam("variables")
        val desc = solver.optimizes match {
          case Some(v) => s"should find optimal value $expectedResult for ${cspom.namesOf(v)}"
          case None if expectedResult == true => s"should be satisfiable"
          case None => s"should have $expectedResult solutions"
        }

        withClue(desc) {

          val f = Future {

            solver.optimizes match {
              case Some(v) =>
                val (_, last) = check(url, solver, declared, solver.optimizes, test)
                last.map(_ (v)) should contain(expectedResult)
              case None =>
                expectedResult match {
                  case b: Boolean =>
                    val solsCut = solver.take(1)
                    val (nbSols, _) = check(url, solsCut, declared, solver.optimizes, test)
                    nbSols > 0 shouldBe b
                  case i: Int =>
                    val solsCut = solver.take(i + 1)
                    val (nbSols, _) = check(url, solsCut, declared, solver.optimizes, test)
                    nbSols shouldBe expectedResult
                }

            }
          }

          Await.result(f, 1000.seconds)
        }

      }


      result.recover {
        case _: UNSATException =>
      }
        .get

    }
  }

  private def compile(parser: Parser, cspomProblem: CSPOM) = {
    parser match {
      case FlatZincFastParser =>
        CSPOMCompiler.compile(cspomProblem, FZPatterns())

      case XCSP3Parser =>
        CSPOMCompiler.compile(cspomProblem, XCSPPatterns())

      case _ =>
        Success(cspomProblem)

    }
  }

  private def check(url: URL, sols: Iterator[CSPOMSolution], variables: Seq[String], opt: Option[CSPOMVariable[_]], prob: Double): (Int, Option[CSPOMSolution]) = {
    val rand = new Random()
    val checker = new XCSP3SolutionChecker(url)
    var nbSol = 0
    var lastSol: Option[CSPOMSolution] = None
    for (sol <- sols) {
      if (rand.nextDouble() < prob) {
        val obj = opt.map(v => sol(v))
        try {
          val failed = checker.checkSolution(sol, obj, variables)
          withClue(XCSP3Concrete.xmlSolution(variables, sol, obj)) {
            failed shouldBe 'empty
          }
        } catch {
          case e: UnsupportedOperationException =>
            logger.error(
              XCSP3Concrete.xmlSolution(variables, sol, obj) +
                "Error in SolutionChecker:", e)
        }
      }
      lastSol = Some(sol)
      nbSol += 1
    }
    (nbSol, lastSol)
  }
}
