package concrete

import java.net.URI
import SQLWriter._
import scala.xml.Node
import scala.xml.Text
import scala.xml.NodeSeq
import java.math.MathContext
import scala.collection.mutable.ListBuffer

object CrossTable extends App {

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

  //3647557
  val version = 1874
  using(SQLWriter.connect(new URI("postgresql://vion:vion@localhost/concrete"))) { connection =>

    val problems = queryEach(connection, """
        SELECT DISTINCT problemId, display, nbvars, nbcons
        FROM executions NATURAL JOIN problems
        WHERE version = %d
        ORDER BY display
        """.format(version)) {
      rs =>
        (rs.getInt(1), rs.getString(2), rs.getInt(3), rs.getInt(4))
    }

    val acv = List(98, 96, 95, 97, 99)
    val acc = List(103, 101, 100, 102, 104)

    val heur = List(85, 64, 83, 84, 70, 72, 59, 71, 87, 86)

    //val nature = List(95, 100)
    val nature = List(134, 135, 136, 137, 138)
    val configs = queryEach(connection, """
        SELECT configId, config
        FROM configs 
        WHERE configId IN (%s) 
        """.format(nature.mkString(", "))) {
      rs => (rs.getInt(1), configDisplay(xml.XML.load(rs.getSQLXML(2).getBinaryStream), rs.getInt(1)))
    } // sortBy (_._2)

    for ((_, desc) <- configs) print(" & " + desc)
    println("\\\\")
    println("\\midrule")

    val cIds = configs map (_._1) mkString (", ")

    var d = Array.ofDim[Int](configs.size, configs.size)

    val statistics = queryEach(connection, """
            SELECT DISTINCT name 
            FROM Statistics NATURAL JOIN Executions
            WHERE version = %d AND configId IN (%s)
    		ORDER BY name
        """.format(version, cIds))(_.getString(1))

    require(statistics.nonEmpty)

    val totals = new Array[Double](configs.size)

    for ((problemId, problem, nbvars, nbcons) <- problems) {

      val data = ListBuffer(problem, nbvars, nbcons)
      //print("\\em %s & \\np{%d} & \\np{%d}".format(problem, nbvars, nbcons))
      //print("\\em %s ".format(problem))

      //val name = List("solver.searchCpu", "filter.revisions", "solver.nbAssignments", "filter.substats.queue.pollSize")(3)

      //val formula = """(cast("filter.substats.queue.pollSize" as real) / cast("filter.substats.queue.nbPoll" as int))"""

      //val formula = """cast("solver.nbAssignments" as real) / cast("solver.searchCpu" as real)"""
      //val formula = """cast("solver.nbAssignments" as real)"""
      //val formula = """cast("solver.nbAssignments" as real) / (cast("solver.searchCpu" as real) + cast("solver.preproCpu" as real))"""
      val formula = """cast("solver.searchCpu" as real) + cast("solver.preproCpu" as real)"""

      //val formula = """cast("concrete.generationTime" as real)"""

      //val formula = """cast("domains.presenceChecks" as bigint)"""

      val min = true

      val sqlQuery = """
            SELECT configId, %s
            FROM crosstab('
    		  SELECT executionId, name, value FROM Statistics NATURAL JOIN Executions
              WHERE version = %d AND configID IN (%s) ORDER BY executionId, name') as ct(executionId int, %s)
              NATURAL JOIN Executions
            WHERE (version, problemId) = (%d, %d)
        """.format(formula, version, cIds, statistics map (s => """"%s" text""".format(s)) mkString (", "), version, problemId)

      //println(sqlQuery)
      val results = queryEach(connection, sqlQuery) {
        rs => rs.getInt(1) -> rs.getDouble(2)
      } toMap

      for (i <- configs.indices; j <- results.get(configs(i)._1)) {
        totals(i) += j
      }

      for (
        i <- configs.indices; j <- configs.indices if i != j;
        val ci = configs(i)._1;
        val cj = configs(j)._1;
        if results.contains(ci)
      ) {
        if (results.contains(cj)) {
          val rj = results(cj)
          val ri = results(ci)
          if ((rj - ri) / math.min(ri, rj) > .1) d(i)(j) += 1
        } else d(i)(j) += 1
      }

      val extrem = results.values.toSeq match {
        case Nil => None
        case d: Seq[Double] => Some(if (min) d.min else d.max)
      }

      configs foreach { c =>
        results.get(c._1) match {
          case Some(e) =>
            val r = e //engineer(e)._1
            //        if (e._2 != "M") throw new IllegalStateException
            //print(" & ")
            //if (extrem.isDefined && (if (min) e < extrem.get * 1.1 else e > extrem.get * .9)) print("\\bf")
            data.append(r) //print("\\np{%.1f}".format(r))
          case None => {
            val sql = """SELECT solution FROM Executions WHERE (version, problemId, configId)=(%d, %d, %d)"""
              .format(version, problemId, c._1)

            val r = query(connection, sql) {
              rs =>
                if (rs.next()) {
                  val r = rs.getString(1)
                  if (r.contains("OutOfMemoryError")) "mem"
                  else r
                } else "---"
            };
            data.append(r)
          }
        }
      }

      println(tabular(data.toSeq))
    }

    println("\\midrule")

    println(configs.indices.map(totals(_)).mkString(" & "))

    println(d map (_.mkString(" ")) mkString ("\n"))
    println()

    schulze(d, configs.map(c => c._1 + "." + c._2).toIndexedSeq)

  }

  def rank(p: Array[Array[Int]], candidates: Seq[Int], cRank: Int = 1, ranking: Map[Int, Seq[Int]] = Map.empty): Map[Int, Seq[Int]] =
    if (candidates.isEmpty) ranking
    else {
      val (win, rem) = candidates.partition { i =>
        candidates.forall { j => p(i)(j) >= p(j)(i) }
      }
      rank(p, rem, cRank + win.size, ranking + (cRank -> win))
    }

  def schulze(d: Array[Array[Int]], labels: IndexedSeq[String]) {

    val p = Array.ofDim[Int](d.size, d.size)

    for (i <- d.indices; j <- d.indices if (i != j)) {
      p(i)(j) =
        if (d(i)(j) > d(j)(i)) d(i)(j) - d(j)(i)
        else 0 //Int.MaxValue
    }

    //    println(p map (_.mkString(" ")) mkString ("\n"))
    //    println()

    for (i <- d.indices; j <- d.indices; k <- d.indices) {
      p(i)(j) = math.max(p(i)(j), math.min(p(i)(k), p(k)(j)))
    }

    //    println(p map (_.mkString(" ")) mkString ("\n"))
    //    println()

    println(rank(p, p.indices).toList.sortBy(_._1) map {
      case (r, c) => "%d: %s".format(r, c.map(labels).mkString(" "))
    } mkString ("\n"))
    //
    //    var candidates: Set[Int] = d.indices.toSet
    //
    //    var rank = 1
    //    while (candidates.nonEmpty) {
    //      print(rank + ". ")
    //      val (win, rem) = candidates.partition { i =>
    //        candidates.forall { j => p(i)(j) >= p(j)(i) }
    //      }
    //      println(win.map(labels).mkString(" "))
    //      rank += win.size
    //      candidates = rem
    //    }

    //    println("graph [ directed 0 ");
    //    for (c <- d.indices) {
    //      println("""node [ id %d label "%s" ]""".format(c, labels(c)))
    //    }
    //
    //    val s = """edge [ source %d target %d label "%d" graphics [ targetArrow "standard" ] ] """
    //
    //    for (i <- d.indices; j <- d.indices if (i != j)) {
    //      if (p(i)(j) > 0) {
    //        println(s.format(i, j, p(i)(j)))
    //      }
    //    }
    //
    //    println("]")
  }

  def configDisplay(desc: NodeSeq, id: Int) = {
    val filter = className(desc \\ "p" filter attributeEquals("name", "mac.filter"))

    val queue = filter match {
      case "ACC" | "AC3Constraint" => className(desc \\ "p" filter attributeEquals("name", "ac3c.queue"))
      case "ACV" | "AC3" => className(desc \\ "p" filter attributeEquals("name", "ac3v.queue"))
      case _ => sys.error("Unknown filter: " + filter)
    }

    val heur = filter match {
      case "ACC" if queue != "QuickFifo" && queue != "SimpleFifos" && queue != "JavaFifo" && queue != "JavaSimpleFifos" =>
        className(desc \\ "p" filter attributeEquals("name", "ac3c.key"))
      case "ACV" if queue != "QuickFifo" =>
        className(desc \\ "p" filter attributeEquals("name", "ac3v.key"))
      case _ => "na"
    }

    val reduction = className(desc \\ "p" filter attributeEquals("name", "reduction"))
    val relation = className(desc \\ "p" filter attributeEquals("name", "relation"))

    "%s-%s-%s-%s-%s (%d)".format(filter, queue, heur, reduction, relation, id)
  }

  def className(n: NodeSeq) = {
    n.headOption match {
      case Some(conf) => conf.text.split('.').last
      case None => "?"
    }
  }

  def engineer(value: Double): (Double, Option[Char]) = {
    if (value == 0) (0, None)
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