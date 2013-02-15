package concrete

import java.net.URI
import SQLWriter._
import scala.xml.Node
import scala.xml.Text
import scala.xml.NodeSeq
import scala.collection.mutable.ListBuffer
import cspfj.StatisticsManager
import java.util.Locale
import scala.collection.mutable.HashMap
import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.slick.session.Database
import Database.threadLocalSession
import scala.slick.jdbc.{ GetResult, StaticQuery => Q }
import scala.slick.driver.PostgresDriver.simple._
import Q.interpolation

object Table extends App {

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
          case d: Int => "\\np{%d}".format(d)
          case _ => "---"
        }.mkString(" & ") + "\\\\"

      case "csv" => data.mkString(", ")

    }
  }

  val version = args.head.toInt

  val nature = args.tail map (_.toInt)

  case class Problem(
    problemId: Int,
    problem: String,
    nbVars: Int,
    nbCons: Int,
    tags: Array[String])

  case class Config(
    configId: Int,
    config: Node) {
    def display = config \\ "p" map (n => className(n)) mkString ("/")
    override def toString = configId + "." + display
  }

  case class Resultat[A](
    solution: String,
    statistic: A)

  implicit val getProblemResult = GetResult(r => Problem(r.<<, r.<<, r.<<, r.<<, r.nextString.split(",")))

  implicit val getConfigResult = GetResult(r => Config(r.<<, xml.XML.loadString(r.nextString)))

  Database.forURL("jdbc:postgresql://localhost/concrete",
    user = "concrete",
    password = "concrete",
    driver = "org.postgresql.Driver") withSession {

      val problems = sql"""
        SELECT problemId, display, nbvars, nbcons, string_agg(tag, ',') as tags
        FROM Problems NATURAL LEFT JOIN ProblemTags
        WHERE problemId IN (
          SELECT problemId 
          FROM Executions
          WHERE version = $version and configId in (#${nature.mkString(",")}))
        GROUP BY problemId, display, nbvars, nbcons
        ORDER BY display
        """.as[Problem].list

      val configs = sql"""
        SELECT configId, config
        FROM configs 
        WHERE configId IN (#${nature.mkString(", ")}) 
        """.as[Config].list

      for (c <- configs) print(" & " + c.display)
      println("\\\\")
      println("\\midrule")

      val cIds = configs map (_.configId) mkString (", ")

      var d = Array.ofDim[Int](configs.size, configs.size)

      val totals = new HashMap[String, Array[List[Double]]]()

      for (Problem(problemId, problem, nbvars, nbcons, tags) <- problems) {

        val data = ListBuffer(problem) //, nbvars, nbcons)
        //print("\\em %s & \\np{%d} & \\np{%d}".format(problem, nbvars, nbcons))
        //print("\\em %s ".format(problem))

        //val name = List("solver.searchCpu", "filter.revisions", "solver.nbAssignments", "filter.substats.queue.pollSize")(3)

        //val formula = """(cast("filter.substats.queue.pollSize" as real) / cast("filter.substats.queue.nbPoll" as int))"""

        //val formula = """cast("solver.nbAssignments" as real) / cast("solver.searchCpu" as real)"""
        ///val formula = """cast("solver.nbAssignments" as real)"""
        //val formula = """cast("solver.nbAssignments" as real) / (cast("solver.searchCpu" as real) + cast("solver.preproCpu" as real))"""
        //val formula = """cast("solver.searchCpu" as real) + cast("solver.preproCpu" as real)"""

        //val formula = """cast("relation.checks" as bigint)"""
        //val formula = """cast("concrete.generationTime" as real)"""

        //val formula = """cast("domains.presenceChecks" as bigint)"""

        //val stat = "cast(stat('solver.nbAssignments', executionId) as int)"

        //val stat = "cast(stat('solver.searchCpu', executionId) as real) + cast(stat('solver.preproCpu', executionId) as real)"

        val min = true

        //            val sqlQuery = """
        //                        SELECT configId, solution, cast(stat('relation.checks', executionId) as real)
        //                        FROM Executions
        //                        WHERE (version, problemId) = (%d, %d)
        //                    """.format(version, problemId)

        val sqlQuery = "mem" match {
          case "mem" => sql"""
                                SELECT configId, solution, cast(stat('solver.usedMem', executionId) as real)
                                FROM Executions
                                WHERE (version, problemId) = ($version, $problemId)
                            """

          case "time" => sql"""
                                SELECT configId, solution, totalTime
                                FROM Times
                                WHERE (version, problemId) = ($version, $problemId)
                            """
                                
          case "nodes" => sql"""
                                SELECT configId, solution, cast(stat('solver.nbAssignments', executionId) as real)
                                FROM Executions
                                WHERE (version, problemId) = ($version, $problemId)
                            """
        }
        

        val results = sqlQuery.as[(Int, String, Double)].list.map {
          case (config, solution, value) => config -> Resultat(solution, value)
        } toMap

        for (
          i <- configs.indices;
          j <- results.get(configs(i).configId);
          k = if (solved(j.solution)) j.statistic else Double.PositiveInfinity;
          tag <- tags
        ) {
          totals.getOrElseUpdate(tag, Array.fill(configs.size)(Nil))(i) ::= k
        }

        for (
          i <- configs.indices; j <- configs.indices if i != j;
          ci = configs(i).configId;
          cj = configs(j).configId;
          ri <- results.get(ci) if solved(ri.solution);
          rj <- results.get(cj)
        ) {
          if (!solved(rj.solution) || {
            val trj = rj.statistic
            val tri = ri.statistic
            (trj - tri) / tri > .1 && (trj - tri) > 1
          }) {
            d(i)(j) += 1
          }
        }

        //      val extrem = results.values.map(_._2).toSeq match {
        //        case Nil => None
        //        case d: Seq[Double] => Some(if (min) d.min else d.max)
        //      }

        configs foreach { c =>
          data.append(
            results.get(c.configId) match {
              case Some(Resultat(result, time)) =>
                if (solved(result)) {
                  //engineer(e)._1
                  //        if (e._2 != "M") throw new IllegalStateException
                  //print(" & ")
                  //if (extrem.isDefined && (if (min) e < extrem.get * 1.1 else e > extrem.get * .9)) print("\\bf")
                  val e = engineer(time)
                  "%.1f%s".formatLocal(Locale.US, e._1, e._2.getOrElse("")) //print("\\np{%.1f}".format(r))
                } else {
                  if (result == null) {
                    "null"
                  } else if (result.contains("OutOfMemoryError")) {
                    "mem"
                  } else if (result.contains("InterruptedException")) {
                    "exp"
                  } else {
                    result
                  }
                }
              case None => "---"
            })
        }

        println(tabular(data.toSeq))
      }

      println("\\midrule")

      //    for ((k, t) <- totals.toList.sortBy(_._1)) {
      //      println(k + " : " + configs.indices.map { i =>
      //        t(i)
      //      }.mkString(" & "))
      //    }

      for ((k, t) <- totals.toList.sortBy(_._1)) {

        val medians = configs.indices.map {
          i =>
            try StatisticsManager.median(t(i))
            catch {
              case e: NoSuchElementException => Double.NaN
            }
        }

        val best = medians.min

        println(s"\\em $k & " + medians.map { median =>
          if (median.isInfinity) {
            "timeout"
          } else {
            val (v, m) = engineer(median)

            (if (median < best * 1.1) "\\bf " else "") + (
              m match {
                case Some(m) => f"\\np[$m%s]{$v%.1f}"
                case None => f"\\np{$v%.1f}"
              })
          }
        }.mkString(" & ") + " \\\\")
      }
      println("\\midrule")

      for ((k, t) <- totals.toList.sortBy(_._1)) {
        val counts = configs.indices.map { i =>
          t(i).count(!_.isInfinity)
        }

        val best = counts.max

        println(s"\\em $k & " + counts.map {
          i => if (i == best) s"\\bf $i" else s"$i"
        }.mkString(" & ") + " \\\\")
      }

      //    println(d.zipWithIndex map { case (r, i) => configs(i) + " " + r.mkString(" ") } mkString ("\n"))
      //    println()

      val labels = configs.map(_.toString).toIndexedSeq

      toGML(d, labels)

      val s = schulze(winnerTakesAll(d))

      println(rank(s, s.indices).toList.sortBy(_._1) map {
        case (r, c) => "%d: %s".format(r, c.map(labels).mkString(" "))
      } mkString ("\n"))
    }

  def solved(solution: String) = solution != null && ("UNSAT" == solution || """^[0-9\ ]*$""".r.findFirstIn(solution).isDefined)

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
      case None => "?"
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
