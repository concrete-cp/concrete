package concrete.runner.sql

import java.net.URL
import java.util

import concrete.ParameterManager
import concrete.generator.cspompatterns.{ConcretePatterns, FZPatterns, XCSPPatterns}
import cspom.{CSPOM, CSPOMGoal}
import cspom.compiler.CSPOMCompiler
import cspom.extension.MDDRelation
import cspom.flatzinc.FlatZincFastParser
import cspom.xcsp.XCSPParser
import mdd.{BDD, SetWithMax}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

object UniversalSet extends Set[Int] {
  override def contains(i: Int): Boolean = true

  override def size: Int = Int.MaxValue

  override def head: Int = 0

  override def +(elem: Int): Set[Int] = ???

  override def -(elem: Int): Set[Int] = ???

  override def iterator: Iterator[Int] = ???
}

/**
  * Created by vion on 10/07/17.
  */
object ProblemStats extends App {
  lazy val DB = Table2.DB

  val pm = new ParameterManager

  val query = DB.run {
    SQLWriter.problems
      .filter(_.d.isEmpty)
      //.filter(_.problemId === 3599)
      .result
  }
  //val result = Await.result(query, Duration.Inf)

  val updates = query.flatMap { r =>
    Future.sequence {
      r.par.map { p =>


        data(new URL(s"file:///home/vion/${p._2}")) match {
          case Success((nature: String, stats: Map[Symbol, Double])) =>
            println(p)
            println(nature)
            println(stats)

            val q = for (u <- SQLWriter.problems if u.problemId === p._1)
              yield (u.nature, u.nbVars, u.d, u.k, u.lambda, u.looseness, u.mddEdges, u.mddVertices, u.bddVertices)
            val action = q.update((Some(nature), stats.get('n).map(_.toInt), stats.get('d).map(_.toInt), stats.get('k),
              stats.get('lambda), stats.get('l), stats.get('mddEdges), stats.get('mddVertices),
              stats.get('bddVertices)))
            DB.run(action)

          case Failure(e) => System.err.println(e)
            Future.successful(())
        }
      }
        .toList
    }
  }

  val r = Await.result(updates, Duration.Inf)
  println(r)

  def data(file: URL): Try[(String, Map[Symbol, Double])] = {

    def parser = file.getFile match {
      case f if f.contains(".xml") => XCSPParser
      case f if f.contains(".fzn") => FlatZincFastParser
    }


    CSPOM.load(file, parser)
      .flatMap { cspomProblem =>
        parser match {
          case FlatZincFastParser =>
            CSPOMCompiler.compile(cspomProblem, FZPatterns())

          case XCSPParser =>
            CSPOMCompiler.compile(cspomProblem, XCSPPatterns())

          case _ => Failure(new IllegalStateException())

        }
      }
      .flatMap { cspom =>
        val nature = cspom.goal.get.obj match {
          case CSPOMGoal.Satisfy => "satisfy"
          case CSPOMGoal.Maximize(e) => s"maximize ${cspom.displayName(e)}"
          case CSPOMGoal.Minimize(e) => s"minimize ${cspom.displayName(e)}"
        }
        CSPOMCompiler.compile(cspom, ConcretePatterns(pm)).map((nature, _))
      }
      .map { case (nature, cspom) =>


        val extensions = for {c <- cspom.constraints
                              if c.getParam("init").contains(false)
                              rel <- c.getParam[MDDRelation]("relation")
                              if rel.arity > 2
        } yield rel

        var d = 0
        var k = 0
        var lambda = 0.0d
        var l = 0.0d
        var bddV = 0l
        var mddE = 0l
        var mddV = 0l

        var count = 0

        for (ext <- extensions) {
          count += 1
          val mddEdges = ext.mdd.edges()
          if (mddEdges > mddE) {
            bddV = BDD(ext.mdd).reduce().vertices()


            // println(ext.hashCode())
            k = ext.arity
            val domains = Array.fill(k)(new util.HashSet[Int])

            ext.mdd.supported(Array.fill(k)(UniversalSet), domains, 0, new SetWithMax(k))


            d = domains.map(_.size).max
            k = domains.length
            lambda = concrete.util.Math.logBigInteger(ext.mdd.lambda())
            val space = domains.map(dom => BigInt(dom.size)).product
            l = Math.exp(lambda - concrete.util.Math.logBigInteger(space))
            mddE = mddEdges
            mddV = ext.mdd.vertices()
          }
        }


        //    println(s"d = $d")
        //    println(s"k = $k")
        //    println(s"lambda = $lambda")
        //    println(s"l = $l")
        //    println(s"e = $e")
        //
        //    println(s"dm = ${dt.toDouble / et}")
        //    println(s"km = ${kt.toDouble / et}")
        //    println(s"lambdam = ${lambdat.doubleValue() / et}")
        //    println(s"lm = ${lt / et}")

        (nature, Map(
          'd -> d,
          'k -> k,
          'lambda -> lambda.doubleValue(),
          'l -> l,
          'mddEdges -> mddE,
          'bddVertices -> bddV,
          'mddVertices -> mddV
        ))
      }
  }

}
