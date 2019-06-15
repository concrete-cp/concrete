package concrete
package it


import java.net.URL

import com.typesafe.scalalogging.LazyLogging
import concrete.generator.cspompatterns.{FZPatterns, XCSPPatterns}
import concrete.runner.{XCSP3Concrete, XCSP3SolutionChecker}
import cspom.CSPOM.Parser
import cspom.compiler.CSPOMCompiler
import cspom.flatzinc.FlatZincFastParser
import cspom.variable.{CSPOMExpression, CSPOMVariable}
import cspom.xcsp.XCSP3Parser
import cspom.{CSPOM, UNSATException}
import org.scalatest._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.{Random, Success, Try}


class SolvingTest extends FunSpec with SolvingBehaviors {

  private val xcsp18COPtest = Seq[(String, (AnyVal, Double))](
    //"NurseRostering-04_c18.xml.lzma" -> ((3062, 1.0)),

    "testMainCOP/Auction-cnt-example_c18.xml.lzma" -> ((54, 1.0)),
    "testMainCOP/Auction-sum-example_c18.xml.lzma" -> ((54, 1.0)),
    "testMainCOP/Bacp-m1-05_c18.xml.lzma" -> ((9, 1.0)),
    "testMainCOP/Bacp-m2-05_c18.xml.lzma" -> ((9, 1.0)),
    "testMainCOP/CrosswordDesign-03-4-rom_c18.xml.lzma" -> ((12, 1.0)),
    // "testMainCOP/CrosswordDesign-10-4-rom_c18.xml.lzma" -> ((0, 1.0)),

    "testMainCOP/Fapp-m2s-03-0300_c18.xml.lzma" -> ((true, 1.0)),
    "testMainCOP/Fapp-m2s-ex1_c18.xml.lzma" -> ((523, 1.0)),
    "testMainCOP/Fapp-m2s-ex2_c18.xml.lzma" -> ((13871, 1.0)),
    "testMainCOP/GolombRuler-a3v18-07_c18.xml.lzma" -> ((25, 1.0)),
    "testMainCOP/GraphColoring-queen5-5_c18.xml.lzma" -> ((4, 1.0)),

    // 10
    "testMainCOP/Knapsack-30-100-00_c18.xml.lzma" -> ((709, 1.0)),
    "testMainCOP/LowAutocorrelation-015_c18.xml.lzma" -> ((15, 1.0)),
    "testMainCOP/Mario-easy-2_c18.xml.lzma" -> ((628, 1.0)),
    "testMainCOP/NurseRostering-00_c18.xml.lzma" -> ((1202, 1.0)),
    "testMainCOP/Pb-circ4-3_c18.xml.lzma" -> ((20, 1.0)),

    "testMainCOP/Pb-garden-4x4_c18.xml.lzma" -> ((4, 1.0)),
    "testMainCOP/PeacableArmies-m1-05_c18.xml.lzma" -> ((4, 1.0)),
    "testMainCOP/PizzaVoucher-10a_c18.xml.lzma" -> ((210, 1.0)),
    "testMainCOP/QuadraticAssignment-qap_c18.xml.lzma" -> ((4776, 1.0)),
    //"testMainCOP/QuadraticAssignment-sko90_c18.xml.lzma" -> ((true, 0)),
    "testMainCOP/Rcpsp-j30-01-01_c18.xml.lzma" -> ((43, 1.0)),

    // 20
    "testMainCOP/Rlfap-graph-03-opt_c18.xml.lzma" -> ((380, 1.0)),
    "testMainCOP/Rlfap-graph-04-opt_c18.xml.lzma" -> ((394, 1.0)),
    "testMainCOP/Rlfap-graph-05-opt_c18.xml.lzma" -> ((true, 1.0)),
    "testMainCOP/Rlfap-graph-10-opt_c18.xml.lzma" -> ((394, 1.0)),
    "testMainCOP/Rlfap-scen-04-opt_c18.xml.lzma" -> ((46, 1.0)),

    // 25
    "testMainCOP/Rlfap-scen-05-opt_c18.xml.lzma" -> ((792, 1.0)),
    "testMainCOP/SteelMillSlab-m1-simple_c18.xml.lzma" -> ((0, 1.0)),
    "testMainCOP/SteelMillSlab-m2-simple_c18.xml.lzma" -> ((0, 1.0)),
    "testMainCOP/StillLife-wastage-08_c18.xml.lzma" -> ((36, 1.0)),
    "testMainCOP/SumColoring-1-fullins-3_c18.xml.lzma" -> ((24, 1.0)),

    // 30
    "testMainCOP/Tal-01_c18.xml.lzma" -> ((6, 1.0)),
    "testMainCOP/TemplateDesign-m1-1_c18.xml.lzma" -> ((2, 1.0)),
    "testMainCOP/TemplateDesign-m2-1_c18.xml.lzma" -> ((2, 1.0)),
    "testMainCOP/TravelingTournament-a2-galaxy04_c18.xml.lzma" -> ((517, 1.0)),
    "testMainCOP/TravellingSalesman-10-20-00_c18.xml.lzma" -> ((47, 1.0)),
  )

  private val xcsp18CSPtest = Seq(
    "testMainCSP/Bibd-sc-07-03-01_c18.xml.lzma" -> ((true, 1.0)),
    "testMainCSP/Bibd-sum-07-03-01_c18.xml.lzma" -> ((true, 1.0)),
    "testMainCSP/CarSequencing-dingbas_c18.xml.lzma" -> ((true, 1.0)),
    "testMainCSP/ColouredQueens-05_c18.xml.lzma" -> ((true, 1.0)),
    "testMainCSP/ColouredQueens-06_c18.xml.lzma" -> ((false, 1.0)),
    //"testMainCSP/ColouredQueens-16_c18.xml.lzma" -> ((,)),
    "testMainCSP/Crossword-m18-ogd2008-vg-04-05_c18.xml.lzma" -> ((true, 1.0)),
    "testMainCSP/Dubois-018_c18.xml.lzma" -> ((false, 1.0)),
    "testMainCSP/Eternity-04-04_c18.xml.lzma" -> ((true, 1.0)),
    "testMainCSP/frb-30-15-1_c18.xml.lzma" -> ((true, 1.0)),
    "testMainCSP/GracefulGraph-K02-P09_c18.xml.lzma" -> ((true, 1.0)),
    "testMainCSP/GracefulGraph-K03-P03_c18.xml.lzma" -> ((true, 1.0)),
    "testMainCSP/Haystacks-ext-04_c18.xml.lzma" -> ((false, 1.0)),
    "testMainCSP/LangfordBin-06_c18.xml.lzma" -> ((false, 1.0)),
    "testMainCSP/MagicHexagon-04-0003_c18.xml.lzma" -> ((true, 1.0)),
    "testMainCSP/MagicHexagon-04-0017_c18.xml.lzma" -> ((false, 1.0)),
    "testMainCSP/MisteryShopper-04_c18.xml.lzma" -> ((true, 1.0)),
    "testMainCSP/Pb-robin08_c18.xml.lzma" -> ((true, 1.0)),
    "testMainCSP/QuasiGroup-4-07_c18.xml.lzma" -> ((false, 1.0)),
    "testMainCSP/QuasiGroup-7-07_c18.xml.lzma" -> ((false, 1.0)),
    "testMainCSP/Rlfap-scen-11-f12_c18.xml.lzma" -> ((false, 1.0)),
    "testMainCSP/SocialGolfers-4-4-4-cp_c18.xml.lzma" -> ((true, 1.0)),
    //"testMainCSP/SocialGolfers-6-6-9-cp_c18.xml.lzma" -> ((,)),
    "testMainCSP/SportsScheduling-08_c18.xml.lzma" -> ((true, 1.0)),
    "testMainCSP/StripPacking-1a_c18.xml.lzma" -> ((false, 1.0)),
    //"testMainCSP/StripPacking-1b_c18.xml.lzma" -> ((,)),
    "testMainCSP/Subisomorphism-g08-g43_c18.xml.lzma" -> ((true, 1.0)),


  )

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
    //"OpenStacks-m2-tiny.xml.xz" -> ((3, 1.0)),
    //"OpenStacks-m2-pb-10-10-1.xml.xz" -> ((5, 1.0)),

    "Rack-r1.xml.xz" -> ((550, 1.0)),
    "Queens-view-8.xml.xz" -> ((92, 1.0)),
    "TestOrdered.xml.xz" -> ((66, 1.0)),
    "TestSumView.xml.xz" -> ((21, 1.0)),
    "CoveringArray-3-04-2-09.xml.xz" -> ((true, 1.0)),

    "GolombRuler-a3-7annot.xml.xz" -> ((25, 1.0)),

    // Long
    "Crossword-lex-vg-4-4.xml.xz" -> ((1068610, 0.001)),
    "Crossword-lex-vg-4-6.xml.xz" -> ((4749, 0.1)),
    "Crossword-lex-vg-4-7.xml.xz" -> ((125, 1.0)),
    "Crossword-lex-vg-4-8.xml.xz" -> ((1, 1.0)),
    "CoveringArray-3-04-2-08.xml.xz" -> ((80640, 0.1)),

    "bdd-15-21-2-2713-79-01.xml.xz" -> ((0, 1.0)),
    "MarketSplit-01.xml.xz" -> ((true, 1.0)),
    "QueenAttacking-05.xml.xz" -> ((0, 1.0)),
    "QueenAttacking-05_X2.xml.xz" -> ((400, 1.0)),
    "Steiner3-07.xml.xz" -> ((151200, .001)),

    //Uses unimplemented circuit constraint
    "Mario-easy-2.xml.xz" -> ((628, 1.0)),
    "Tpp-3-3-20-1.xml.xz" -> ((126, 1.0))
  )


  private val problemBank = Seq[(String, (AnyVal, Boolean))](
    //  "celar-CELAR6-SUB2.fzn.xz" -> ((true, false)),
    "mznc18-err/error_bool2.fzn" -> ((2, false)),
    "mznc18-err/error_empty.fzn" -> ((true, false)),
    "mznc18-err/error_key.fzn" -> ((true, false)),
    "mznc18-err/error_bool.fzn" -> ((2, false)),
    "mznc18-err/error_illarg.fzn" -> ((true, false)),
    "mznc18-err/error_mod.fzn" -> ((8, false)),

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

  private val pm = new ParameterManager()
  private val parameters = pm //.updated("heuristic.variable", classOf[LastConflict] +: VariableHeuristic.default) //.updated("f", Unit).updated("heuristic.value", classOf[BestCost])

  for {
    (p, (r, test)) <- Seq(
      xcsp18COPtest,
      xcsp18CSPtest,
      lecoutrePB,
      problemBank,
    ).flatten
  } {

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
    "QueenAttacking-04_X2.xml.xz",
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


      val result: Try[Unit] = for {

        parser <- Try(CSPOM.autoParser(url).get)
        cspom <- CSPOM.load(url, parser).flatMap(compile(parser, _))
        //        cspom <- FZConcrete.loadCSPOMURL(pm, url)
        solver <- Solver(cspom, pm)

      } yield {


        val declared = cspom.goal.get.getSeqParam[(String, CSPOMExpression[_])]("variables").map(_._1)
        val desc = solver.optimizes match {
          case Some(v) => s"should find optimal value $expectedResult for ${cspom.namesOf(v)}"
          case None if expectedResult == true => s"should be satisfiable"
          case None => s"should have $expectedResult solutions"
        }

        withClue(desc) {

          val f = Future {

            solver.optimizes match {
              case Some(v) =>
                expectedResult match {
                  case b: Boolean =>
                    val solsCut = solver.take(1)
                    val (_, last) = check(url, solsCut, declared, solver.optimizes, test)
                    last.isDefined shouldBe b
                  case i: Int =>
                    val (_, last) = check(url, solver, declared, solver.optimizes, test)
                    last.map(_ (v)) should contain(i)
                }

              case None =>
                expectedResult match {
                  case b: Boolean =>
                    val solsCut = solver.take(1)
                    val (nbSols, _) = check(url, solsCut, declared, solver.optimizes, test)
                    nbSols > 0 shouldBe b
                  case i: Int =>
                    val solsCut = solver.take(i + 1)
                    //                    solsCut.foreach { s =>
                    //                      println(FZConcrete.outputCSPOM(cspom.expressionMap, s, None))
                    //                    println("=========")}

                    val (nbSols, _) = check(url, solsCut, declared, solver.optimizes, test)

                    nbSols shouldBe expectedResult
                }

            }
          }

          Await.result(f, 1000.seconds)
        }

      }


      result.recover {
        case e: UNSATException =>
          if (expectedResult == true || expectedResult != 0) {
            fail("Encountered UNSATException on satisfiable problem", e)
          } else {
            ()
          }
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
            failed shouldBe empty
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
