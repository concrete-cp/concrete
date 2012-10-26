package concrete

import java.net.URI
import SQLWriter._
import scala.xml.Node
import scala.xml.Text
import scala.xml.NodeSeq
import java.math.MathContext

object Table extends App {

  def attributeEquals(name: String, value: String)(node: Node) = {
    node.attribute(name).get.exists(_ == Text(value))
  }
  //3647557
  val version = 1833
  using(SQLWriter.connect(new URI("postgresql://localhost/concrete"))) { connection =>

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

    val configs = queryEach(connection, """
        SELECT configId, config
        FROM configs 
        WHERE configId IN (%s)
        """.format((acv ++ acc).mkString(", "))) {
      rs => (rs.getInt(1), configDisplay(xml.XML.load(rs.getSQLXML(2).getBinaryStream)))
    }

    for ((_, desc) <- configs) print(" & " + desc)
    println("\\\\")
    println("\\midrule")

    var d = Array.ofDim[Int](configs.size, configs.size)

    for ((problemId, problem, nbvars, nbcons) <- problems) {

      //print("\\em %s & \\np{%d} & \\np{%d}".format(problem, nbvars, nbcons))
      print("\\em %s ".format(problem))

      val name = List("solver.searchCpu", "filter.revisions", "solver.nbAssignments", "filter.substats.queue.pollSize")(0)

      val results = configs map {
        case (configId, _) =>
          queryEach(connection, """
            SELECT value 
            FROM Statistics NATURAL JOIN Executions
            WHERE (version, configId, problemId) = (%d, %d, %d)
              AND name = '%s'
        """.format(version, configId, problemId, name))(_.getDouble(1)) match {
            case List(d) => d
            case _ => Double.NaN
          }
      }

      for (i <- configs.indices; j <- configs.indices if (i != j)) {
        if ((results(j) - results(i)) / math.min(results(i), results(j)) > .1) d(i)(j) += 1
      }

      val min = results.min

      results.foreach { e =>
        val r = e //engineer(e)._1
        //        if (e._2 != "M") throw new IllegalStateException
        print(" & ")
        if (e < min * 1.1) print("\\bf")
        print("\\np{%.1f}".format(r))
      }

      println("\\\\")
    }

    println("\\midrule")

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

  def configDisplay(desc: NodeSeq) = {
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

    filter + "-" + queue + "-" + heur
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