package concrete.runner.sql

import scala.xml.Node
import scala.xml.Text
import scala.xml.NodeSeq

import scala.collection.mutable.HashMap
import scala.annotation.tailrec
import scala.collection.SortedMap
import slick.jdbc.PostgresProfile.api._
import slick.jdbc.GetResult

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import com.typesafe.config.ConfigFactory
import java.io.File

object Table2 extends App {

  val format = "csv"

  def attributeEquals(name: String, value: String)(node: Node) = {
    node.attribute(name).get.exists(_ == Text(value))
  }

  def tabular(data: Seq[Any]) = {
    format match {
      case "latex" =>
        data.map {
          case d: String => d
          case d: Double => "\\np{%.1f}".format(d)
          case d: Int    => "\\np{%d}".format(d)
          case _         => "---"
        }.mkString(" & ") + "\\\\"

      case "csv" => data.mkString(", ")

    }
  }

  lazy val baseConfig = ConfigFactory.load //defaults from src/resources

  lazy val systemConfig = Option(System.getProperty("concrete.config")) match {
    case Some(cfile) => ConfigFactory.parseFile(new File(cfile)).withFallback(baseConfig)
    case None        => baseConfig
  }

  lazy val DB =
    Database.forConfig("database", systemConfig)

  val statistic :: version :: nature = args.toList

  // Set display :
  // update "Problem" set display = substring(name from '%/#"[^/]+#".xml.bz2' for '#')

  case class Resultat(
      status: String,
      statistic: Double) {
    def solved: Failure = {
      if (status == "SAT" || status == "UNSAT") {
        Success
      } else if (status.contains("Timeout")) {
        Timeout
      } else if (status.contains("OutOfMemory") || status.contains("relation is too large")) {
        OOM
      } else if ("started" == status) {
        Stalled
      } else {
        UnknownError
      }
    }

    def toString(toHandler: ErrorHandling, oomHandler: ErrorHandling): String = {
      solved match {
        case Success => statistic.toString
        case Timeout => s"${timeoutHandler.toDouble(statistic)} (TO)"
        case OOM     => s"${oomHandler.toDouble(statistic)} (OOM)"
        case Stalled => s"${oomHandler.toDouble(statistic)} (Stalled)"
        case _       => s"${Double.NaN} ($status)"
      }
    }

    def toDouble(toHandler: ErrorHandling, oomHandler: ErrorHandling): Double = {
      solved match {
        case Success       => statistic
        case Timeout       => timeoutHandler.toDouble(statistic)
        case OOM | Stalled => oomHandler.toDouble(statistic)
        case _             => Double.NaN
      }
    }
  }

  case class Problem(
    problemId: Int,
    problem: String,
    nbVars: Int,
    nbCons: Int,
    _tags: String) {
    lazy val tags = _tags.split(",")
  }

  case class Execution(
    problemId: Int,
    configId: Int,
    iteration: Int,
    status: String,
    statistic: Option[Double])

  sealed trait Failure
  case object Success extends Failure
  case object Timeout extends Failure
  case object OOM extends Failure
  case object Stalled extends Failure
  case object UnknownError extends Failure

  val configs = nature.map(_.toInt)
  val configsMap = configs.zipWithIndex.toMap

  //  {
  //    val configsQ = sql"""
  //        SELECT "configId", config
  //        FROM "Config" 
  //        WHERE "configId" IN (#${nature.mkString(", ")}) 
  //        """.as[(Int, String)]
  //
  //    val configDisplay = new Array[String](nature.size)
  //
  //    val configs = Await.result(DB.run(configsQ), Duration.Inf)
  //
  //    for ((cId, display) <- configs) configDisplay(configsMap(cId)) = display
  //    println("\t" + configDisplay.mkString("\t"))
  //  }

  implicit val getProblemResult = GetResult(r => Problem(r.<<, r.<<, r.<<, r.<<, r.<<))

  //var d = Array.ofDim[Int](configs.size, configs.size)

  val timeoutHandler: ErrorHandling = statistic match {
    case "rps"          => ErrorKeep
    case "nodes"        => ErrorCap(Double.PositiveInfinity)
    case "time"         => ErrorKeep
    case "revisions"    => ErrorKeep
    case "mem"          => ErrorKeep
    case "domainChecks" => ErrorCap(Double.PositiveInfinity)
    case "nps"          => ErrorKeep
  }

  val oomHandler: ErrorHandling = statistic match {
    case "rps"          => ErrorCap(0)
    case "nodes"        => ErrorCap(Double.PositiveInfinity)
    case "time"         => ErrorCap(1200)
    case "revisions"    => ErrorCap(0)
    case "mem"          => ErrorCap(Double.PositiveInfinity)//4 * math.pow(2, 10))
    case "domainChecks" => ErrorCap(Double.PositiveInfinity)
    case "nps"          => ErrorCap(0)
  }

  // val ignoreNaN = true

  val aggregator = {
    data: Seq[Double] => if (data.isEmpty) -1 else data.sum
  }

  val statQuery = statistic match {
    case "mem"          => """cast(stat('solver.usedMem', "executionId") as real)/1048576.0"""
    case "time"         => """totalTime('{solver.searchCpu, solver.preproCpu}', "executionId")/1e3"""
    case "nodes"        => """cast(stat('solver.nbAssignments', "executionId") as real)"""
    case "domainChecks" => """cast(stat('solver.domain.checks', "executionId") as real)"""
    case "nps"          => """1e3*cast(stat('solver.nbAssignments', "executionId") as real)/nullif(totalTime('{solver.searchCpu, solver.preproCpu}', "executionId"), 0.0)"""
    case "rps"          => """cast(stat('solver.filter.revisions', "executionId") as real)/nullif(totalTime('{solver.searchCpu, solver.preproCpu}', "executionId"), 0.0)"""
    case "revisions"    => """cast(stat('solver.filter.revisions', "executionId") as bigint)"""
  }

  implicit val getExecutionResult = GetResult(r => Execution(r.<<, r.<<, r.<<, r.<<, r.<<))

  val pe = DB.run(sql"""
        SELECT "problemId", display, "nbVars", "nbCons", string_agg("problemTag", ',') as tags
        FROM "Problem" NATURAL LEFT JOIN "ProblemTag"
        WHERE "problemId" IN (
          SELECT "problemId" 
          FROM "Execution"
          WHERE version = $version and "configId" in (#${nature.mkString(",")}))
          -- AND "problemTag" = 'modifiedRenault'
        GROUP BY "problemId"
        """.as[Problem])
    .map { p =>
      p.map(q => q.problemId -> q).toMap
    }
    .flatMap {
      case p =>

        //  problems.onSuccess { case r => println(r) }

        DB.run(sql"""
    SELECT "problemId", "configId", iteration, status, #$statQuery 
    FROM "Execution"
    WHERE version = $version AND "configId" IN (#${nature.mkString(", ")})
    """.as[Execution])
          .map {
            e => (p, e)
          }

    }

  val fut = for ((p, e) <- pe) yield {
    // Key is (problemTag, 
    val totals = HashMap[String, Map[Int, List[Double]]]().withDefaultValue(Map().withDefaultValue(Nil))

    // Key is (problemId, iteration), value maps configId to result
    val eresults = Map[(Int, Int), Map[Int, Resultat]]().withDefaultValue(Map())

    val results = e.foldLeft(eresults) {
      case (res, Execution(problemId, configId, iteration, status, statistic)) =>
        val r: Resultat = Resultat(status, statistic.getOrElse(Double.NaN))
        val oldRes = res((problemId, iteration))
        res.updated((problemId, iteration), oldRes + (configId -> r))
    }

    for {
      ((problemId, iteration), stats) <- results
      if !stats.forall(_._2.toDouble(timeoutHandler, oomHandler).isNaN)
      tag <- p(problemId).tags
      (conf, result) <- stats
    } {
      val old = totals(tag)(conf)
      totals(tag) += conf -> (result.toDouble(timeoutHandler, oomHandler) :: old)
    }

    (results, totals, p)
  }

  val (results, totals, pbs) = Await.result(fut, Duration.Inf)

  for (((problemId, iteration), stats) <- results.toSeq.sortBy { p => (pbs(p._1._1).problem, p._1._2) }) {
    print(s"${problemId}. ${pbs(problemId).problem} $iteration\t")
    println(configs.map(c => stats.getOrElse(c, Resultat("not started", Double.NaN))).map(_.toString(timeoutHandler, oomHandler)).mkString("\t"))
  }

  val ignoreNaN = false

  for ((k, t) <- totals.toSeq.sortBy(_._1)) {
    //println(s"$k: $t")
    val medians = configs.map(t).map {
      l =>
        try {
          if (ignoreNaN) {
            aggregator(l.filterNot(_.isNaN))
          } else if (l.exists(_.isNaN)) {
            Double.NaN
          } else {
            aggregator(l)
          }
        } catch {
          case e: NoSuchElementException => Double.NaN
        }

    }

    println(s"$k\t" + medians.map { median =>
      f"${median}%f"
      //            val (v, m) = engineer(median)
      //
      //            (if (median < best * 1.1) "\\bf " else "") + (
      //              m match {
      //                case Some(m) => f"\\np[$m%s]{$v%.1f}"
      //                case None => f"\\np{$v%.1f}"
      //              })
    }.mkString("\t")) // + " \\\\")

  }

  //println("\\midrule")

  //      for ((k, counts) <- nbSolved.toList.sortBy(_._1)) {
  //        val best = counts.max
  //
  //        println(s"\\em $k & " + counts.map {
  //          i => if (i == best) s"\\bf $i" else s"$i"
  //        }.mkString(" & ") + " \\\\")
  //      }

  //    println(d.zipWithIndex map { case (r, i) => configs(i) + " " + r.mkString(" ") } mkString ("\n"))
  //    println()

  //      val labels = configs.map(_.toString).toIndexedSeq
  //
  //      toGML(d, labels)
  //
  //      val s = schulze(winnerTakesAll(d))
  //
  //      println(rank(s, s.indices).toList.sortBy(_._1) map {
  //        case (r, c) => "%d: %s".format(r, c.map(labels).mkString(" "))
  //      } mkString ("\n"))

  // solution != null && ("UNSAT" == solution || """^[0-9\s]*$""".r.findFirstIn(solution).isDefined || "^Map".r.findFirstIn(solution).isDefined)

  @tailrec
  def rank[B <% Ordered[B]](p: IndexedSeq[Array[B]], candidates: Seq[Int],
                            cRank: Int = 1, ranking: SortedMap[Int, Seq[Int]] = SortedMap.empty): SortedMap[Int, Seq[Int]] =
    if (candidates.isEmpty) {
      ranking
    } else {
      val (win, rem) = candidates.partition { i =>
        candidates.forall { j => p(i)(j) >= p(j)(i) }
      }
      if (win.isEmpty) ranking
      else {
        //println(cRank + " : " + win)
        rank(p, rem, cRank + win.size, ranking + (cRank -> win))
      }
    }

  def winnerTakesAll(d: Array[Array[Int]]) = {
    val p = Array.ofDim[Int](d.length, d.length)
    for (i <- d.indices.par; j <- d.indices) {
      if (d(i)(j) > d(j)(i)) {
        p(i)(j) = d(i)(j)
      }
    }
    p
  }

  def schulze[A <% Ordered[A]](p0: Array[Array[A]]) = {
    //val p = percentages(d, avis)

    val p = p0.map(_.clone)

    for (i <- p.indices) {
      //println("%.0f %%".format(100.0 * i / p.length))

      for (j <- p.indices if i != j) {
        var k = 0;
        while (k < p.length) {
          if (i != k && j != k) {
            p(j)(k) = max(p(j)(k), min(p(j)(i), p(i)(k)))
          }
          k += 1
        }
      }
    }

    //p.map(_.mkString(" ")).foreach(println)

    p

    //toGML(d,labels)
  }

  private def min[A <% Ordered[A]](a: A, b: A): A =
    if (a < b) a else b

  private def max[A <% Ordered[A]](a: A, b: A): A =
    if (a > b) a else b

  private def toGML(p: Array[Array[Int]], labels: IndexedSeq[String]) {
    println("graph [ directed 0 ");
    for (c <- p.indices) {
      println("""node [ id %d label "%s" ]""".format(c, labels(c)))
    }

    val s = """edge [ source %d target %d label "%d" graphics [ targetArrow "standard" ] ] """

    for (i <- p.indices; j <- p.indices if (i != j)) {
      if (p(i)(j) > 0) {
        println(s.format(i, j, p(i)(j)))
      }
    }

    println("]")
  }

  def className(n: NodeSeq) = {
    n.headOption match {
      case Some(conf) => conf.text.split('.').last
      case None       => "?"
    }
  }

  def engineer(value: Double): (Double, Option[Char]) = {
    if (value == 0 || value.isInfinity) { (value, None) }
    else {
      val CONSTANTS = Map(
        3 -> Some('G'),
        2 -> Some('M'),
        1 -> Some('k'),
        0 -> None,
        -1 -> Some('m'),
        -2 -> Some('u'),
        -3 -> Some('n'));

      val level = math.floor(math.log10(math.abs(value)) / 3).toInt;

      (value * math.pow(10, -3 * level), CONSTANTS(level));
    }
  }

}
