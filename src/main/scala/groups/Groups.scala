package groups

import scala.Array.canBuildFrom
import concrete.CSPOMDriver._
import concrete.ParameterManager
import concrete.ParameterManager
import concrete.Solver
import concrete.Variable
import concrete.heuristic.value.Lexico
import cspom.CSPOM
import cspom.CSPOM._
import cspom.CSPOMGoal.Maximize
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable

object Groups extends App {
  //ParameterManager("logger.level") = "INFO"
  val pm = new ParameterManager
  //pm("heuristic.value") = classOf[concrete.heuristic.RevLexico]
  //ParameterManager("heuristic.variable") = classOf[concrete.heuristic.LexVar]

  val data2 = Seq(
    "VANDAELE Vivien" -> 1651,
    "POREZ Kévin" -> 1434,
    "ESCUDIER Arthur" -> 1338,
    "BOUAGGAD Abdessamade" -> 1290,
    "BODIANG Aboubacar" -> 1259,
    "ACHOUR Zackaria" -> 1210,
    "HILBERER Maxime" -> 1181,
    "HOURIEZ Paul" -> 1101,
    "GRAF Julien" -> 1071,
    "DJELLOULI Brahim" -> 1050,
    "DELVALLÉE Clément" -> 1050,
    "LEBAS Edouard" -> 1015,
    "BARTHÉLÉMY Florian" -> 999,
    "ALTMANN Anthony" -> 985,

    "CLIMPONT Valentin" -> 1417,
    "GUEGAIN Edouard" -> 1334,
    "LEJEUNE Valentine" -> 1300,
    "LEGRAND Adrien" -> 1234,
    "WALBERT Julien" -> 1228,
    "FROMENTIN Robin" -> 1207,
    "BRZYCKI Corentin" -> 1182,
    "LECERF Valentin" -> 1163,
    "NAWROCKI Florentin" -> 1125,
    "LANVIN Dylan" -> 1123,
    "WAUQUIER Victor" -> 1073,
    "TSOUMBOU MOUTIMBA Robert" -> 1047,
    "TANG Lian" -> 1044,
    "BAK Antoine" -> 1030,

    "GERIS Nicolas" -> 1438,
    "DECROIX Dorian" -> 1420,
    "SCULIER Loïc" -> 1208,
    "PENNACCHIOLI Valentin" -> 1124,
    "DEFOSSEZ Anthony" -> 1179,
    "HUANG Ying" -> 1103,
    "DECOUVELAERE Florian" -> 1026,
    "ZHANG Sai" -> 1019,
    "VERRIEZ Karim" -> 1003)

  def a1 = data2.take(14)

  def a2 = data2.slice(14, 28)

  def b1 = data2.slice(28, 37)

  //def b2 = data2.slice(37,43)

  def risque = Seq()
  require(risque.forall(data2.contains))

  val data = a1.toSeq.sortBy(_._1)

  val (noms, notes) = data.toList.unzip

  println(noms)

  val nbs = data.size
  val nbg4 = 1
  val nbg5 = 2
  val nbg6 = 0
  //val nbg = nbs / minGroupSize

  require(nbs == nbg4 * 4 + nbg5 * 5 + nbg6 * 6, "Nombre de groupes incohérent")

  val problem = CSPOM { implicit problem =>
    val groups4 = for (g <- 0 until nbg4) yield for (s <- 0 until nbs) yield {
      IntVariable(0, 1) as s"G4${g}S${s}"
    }
    val groups5 = for (g <- 0 until nbg5) yield for (s <- 0 until nbs) yield {
      IntVariable(0, 1) as s"G5${g}S${s}"
    }
    val groups6 = for (g <- 0 until nbg6) yield for (s <- 0 until nbs) yield {
      IntVariable(0, 1) as s"G6${g}S${s}"
    }
    val minMoy = IntVariable(0 to (1 + notes.sum / notes.size)) as "minMoy"

    ctr(minMoy * notes.size <= notes.sum)

    //    println(minMoy)

    for (g <- groups4) {
      ctr(occurrence(1)(g: _*) === 4)
    }
    for (g <- groups5) {
      ctr(occurrence(1)(g: _*) === 5)
    }
    for (g <- groups6) {
      ctr(occurrence(1)(g: _*) === 6)
    }

    // Chaque étudiant dans un seul groupe
    for (s <- 0 until nbs) {
      val group = (groups4 ++ groups5 ++ groups6) map { g => g(s) }
      ctr(occurrence(1)(group: _*) === 1)
    }

    for (g <- 0 until nbg4) {
      val v = sumProd(notes zip groups4(g): _*) as s"GSum4$g"
      ctr(v >= minMoy * 4)
    }

    for (g <- 0 until nbg5) {
      val v = sumProd(notes zip groups5(g): _*) as s"GSum5$g"
      ctr(v >= minMoy * 5)
    }

    for (g <- 0 until nbg6) {
      val v = sumProd(notes zip groups6(g): _*) as s"GSum6$g"
      ctr(v >= minMoy * 6)
    }

    //Symétries

    for (g <- 0 until nbg4 - 1) {
      ctr(seq2CSPOMSeq(groups4(g + 1)) <= groups4(g))
    }
    for (g <- 0 until nbg5 - 1) {
      ctr(seq2CSPOMSeq(groups5(g + 1)) <= groups5(g))
    }
    for (g <- 0 until nbg6 - 1) {
      ctr(seq2CSPOMSeq(groups6(g + 1)) <= groups6(g))
    }
    val incompatibilites = List[Seq[String]](
      Seq("DJELLOULI Brahim", "HOURIEZ Paul"),
      Seq("DELVALLÉE Clément", "POREZ Kévin"),
      Seq("WALBERT Julien", "LEGRAND Adrien"),
      Seq("NAWROCKI Florentin", "FROMENTIN Robin"),
      Seq("LANVIN Dylan", "FROMENTIN Robin"),
      Seq("BRZYCKI Corentin", "LEGRAND Adrien"),
      Seq("WAUQUIER Victor", "TANG Lian"),
      Seq("DEFOSSEZ Anthony", "VERRIEZ Karim"),
      Seq("VANDAELE Vivien", "BOUAGGAD Abdessamade"),
      Seq("VANDAELE Vivien", "BARTHÉLÉMY Florian"),
      Seq("CLIMPONT Valentin", "FROMENTIN Robin"),
      Seq("LECERF Valentin", "FROMENTIN Robin"))
//      ,
//      Seq("LEBAS Edouard", "DJELLOULI Brahim"))
    //Seq("VERRIEZ Karim", "BACHABI")

    require(incompatibilites.flatten.forall(data2.map(_._1).contains),
      incompatibilites.flatten.find(!data2.map(_._1).contains(_)))
    //Incompatibilités
    for (
      inc <- incompatibilites
    ) {
      val i0 = noms.indexOf(inc(0))
      val i1 = noms.indexOf(inc(1))
      if (i0 >= 0 && i1 >= 0) {
        for (g <- groups4 ++ groups5 ++ groups6) {
          ctr(g(i0) + g(i1) <= 1)
        }
      }
    }

    //Un risque par groupe
    val ri = risque.map(noms.indexOf).filter(_ >= 0)
    if (ri.size >= 2) {
      for (
        g <- groups4 ++ groups5 ++ groups6
      ) {
        ctr(occurrence(1)(ri.map(g): _*) === 1)
      }
    }

    // Plez dans un groupe de 5
    val i = noms.indexOf("ALTMANN Anthony")
    require(i >= 0)
    val plez = for (g <- groups5) yield g(i)
    ctr(sum(plez: _*) >= 1)

    goal(Maximize(minMoy))
  }

  //println(problem)

  val s = Solver(problem, pm).get

  //println(s.cspom)

  // s.maximize(problem.variable("minMoy").get)

  for (sol <- s) {
    println(sol("minMoy"))
    for (g <- 0 until nbg4) {
      print(sol(s"GSum4$g").asInstanceOf[Int] / 4 + ": ")
      //println((0 until nbs).map(s => sol(s"G4${g}S$s")))
      println((0 until nbs).filter(s => sol(s"G4${g}S$s") == true).map(s => noms(s) + "(" + notes(s) + ")").mkString(", "))
    }
    for (g <- 0 until nbg5) {
      print(sol(s"GSum5$g").asInstanceOf[Int] / 5 + ": ")
      println((0 until nbs).filter(s => sol(s"G5${g}S$s") == true).map(s => noms(s) + "(" + notes(s) + ")").mkString(", "))
    }
    for (g <- 0 until nbg6) {
      print(sol(s"GSum6$g").asInstanceOf[Int] / 6 + ": ")
      println((0 until nbs).filter(s => sol(s"G6${g}S$s") == true).map(s => noms(s) + "(" + notes(s) + ")").mkString(", "))
    }
    for (g <- 0 until nbg4) {
      println((0 until nbs).filter(s => sol(s"G4${g}S$s") == true).map(s => noms(s)).mkString(", "))
    }
    for (g <- 0 until nbg5) {
      println((0 until nbs).filter(s => sol(s"G5${g}S$s") == true).map(s => noms(s)).mkString(", "))
    }
    for (g <- 0 until nbg6) {
      println((0 until nbs).filter(s => sol(s"G6${g}S$s") == true).map(s => noms(s)).mkString(", "))
    }
    println("--------")
  }
}
