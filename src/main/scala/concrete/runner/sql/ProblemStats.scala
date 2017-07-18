package concrete.runner.sql

import java.net.URL

import bitvectors.BitVector
import concrete.ParameterManager
import concrete.generator.cspompatterns.{ConcretePatterns, FZPatterns, XCSPPatterns}
import cspom.CSPOM
import cspom.compiler.CSPOMCompiler
import cspom.extension.MDDRelation
import cspom.flatzinc.FlatZincFastParser
import cspom.xcsp.XCSPParser
import mdd.{BDD, MiniSet, SetWithMax}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Failure

object UniversalSet extends MiniSet {
  override def present(i: Int): Boolean = true

  override def size: Int = Int.MaxValue

  override def head: Int = 0
}

/**
  * Created by vion on 10/07/17.
  */
object ProblemStats extends App {
  lazy val DB = Table2.DB

  val pm = new ParameterManager

  val query = DB.run {
    SQLWriter.problems
      .filter(_.nbVars.isEmpty)
      //.filter(_.problemId === 3599)
      .result
  }
  //val result = Await.result(query, Duration.Inf)

  val updates = query.flatMap { r =>
    Future.sequence {
      r.par.map { p =>
        println(p)
        val stats = data(new URL(s"file:///home/vion/xp-table/${p._2}"))

        println(stats)

        val q = for (u <- SQLWriter.problems if u.problemId === p._1)
          yield (u.nbVars, u.d, u.k, u.lambda, u.looseness, u.mddEdges, u.mddVertices, u.bddVertices)
        val action = q.update((stats.get('n).map(_.toInt), stats.get('d).map(_.toInt), stats.get('k),
          stats.get('lambda), stats.get('l), stats.get('mddEdges), stats.get('mddVertices),
          stats.get('bddVertices)))
        DB.run(action)
      }
        .toList
    }
  }

  val r = Await.result(updates, Duration.Inf)
  println(r)

  def data(file: URL): Map[Symbol, Double] = {

    def parser = file.getFile match {
      case f if f.contains(".xml") => XCSPParser
      case f if f.contains(".fzn") => FlatZincFastParser
    }

    val cspom = CSPOM.load(file, parser)
      .flatMap { cspomProblem =>
        parser match {
          case FlatZincFastParser =>
            CSPOMCompiler.compile(cspomProblem, FZPatterns())

          case XCSPParser =>
            CSPOMCompiler.compile(cspomProblem, XCSPPatterns())

          case _ => Failure(new IllegalStateException())

        }
      }
      .flatMap(
        CSPOMCompiler.compile(_, ConcretePatterns(pm)))
      .get

    val extensions = for {c <- cspom.constraints
                          if c.getParam("init").contains(false)
                          rel <- c.getParam[MDDRelation]("relation")
                          if (rel.arity > 2)
    } yield rel

    var d = 0
    var k = 0
    var lambda = BigInt(0)
    var l = 0.0d
    var bddV = 0l
    var mddE = 0l
    var mddV = 0l

    var dt = 0
    var kt = 0
    var lambdat = BigInt(0)
    var lt = 0.0d
    var bddVt = 0l
    var mddEt = 0l
    var mddVt = 0l

    var count = 0

    for (ext <- extensions) {
      count += 1
      val mddEdges = ext.mdd.edges()
      val bddVertices = BDD(ext.mdd).reduce().vertices()


      // println(ext.hashCode())
      val arity = ext.arity
      val domains = Array.fill(arity)(BitVector.empty)

      ext.mdd.supported(Array.fill(arity)(UniversalSet), domains, 0, new SetWithMax(arity))


      val ds = domains.map(_.cardinality).max

      d = math.max(d, ds)
      dt += ds * mddEdges


      k = math.max(k, domains.size)
      kt += domains.size * mddEdges

      val lambd = ext.mdd.lambda()
      lambda = lambda max lambd
      lambdat += lambd * mddEdges

      // println(domains)

      //    println(s"k, e = ${domains.size}, $edges")

      val space = domains.map(dom => BigInt(dom.cardinality)).reduce(_ * _)
      val loose = lambd.doubleValue() / space.doubleValue()
      l = math.max(l, loose)
      lt += loose * mddEdges
      //      println(s"l = ${lambd.doubleValue} / ${space.doubleValue} = ${loose}")

      mddE = math.max(mddE, mddEdges)
      mddEt += mddEdges
      bddV = math.max(bddV, bddVertices)
      bddVt += bddVertices
      val mddVertices = ext.mdd.vertices()
      mddV = math.max(mddV, mddVertices)
      mddVt += mddVertices
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

    Map(
      'n -> cspom.referencedExpressions.count(_.searchSpace > 1),
      'd -> d,
      'k -> kt.toDouble / mddEt,
      'lambda -> lambdat.doubleValue() / mddEt,
      'l -> lt / mddEt,
      'mddEdges -> mddEt.toDouble / count,
      'bddVertices -> bddVt.toDouble / count,
      'mddVertices -> mddVt.toDouble / count
    )
  }
}
