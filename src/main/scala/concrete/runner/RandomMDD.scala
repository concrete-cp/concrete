package concrete.runner
import scala.util.Random
import cspom.CSPOM
import rb.randomlists.CoarseProportionRandomListGenerator
import rb.randomlists.Structure
import scala.slick.jdbc.StaticQuery.interpolation
import scala.slick.session.Database
import Database.threadLocalSession
import java.net.URI
import cspom.StatisticsManager
import scala.collection.mutable.HashMap
import cspom.extension.EmptyMDD
import cspom.extension.MDDLeaf
import cspom.extension.MDDNode
import cspom.extension.MDD
import cspom.extension.LazyMDD
import cspom.CSPOM._
import scala.Array.canBuildFrom
import scala.slick.jdbc.{ StaticQuery => Q }
import cspom.variable.IntVariable

object RandomMDD extends ConcreteRunner with App {

  def apply(d: Int, k: Int, l: Double, q: Double, rand: Random) = {
    val existing = Array.fill(k + 1)(new HashMap[MDD, MDD]())
    generate(d, k, l, q, existing, rand)
  }

  def generate(d: Int, k: Int, l: Double, q: Double,
    existing: Array[HashMap[MDD, MDD]], rand: Random): MDD = {
    if (k == 0) {
      if (rand.nextDouble < l) {
        MDDLeaf
      } else {
        EmptyMDD
      }
    } else if (existing(k).nonEmpty && rand.nextDouble < q) {
      existing(k).keysIterator.drop(rand.nextInt(existing(k).size)).next
    } else {
      val t = (0 until d).iterator.
        map(i => i -> generate(d, k - 1, l, q, existing, rand)).
        filter(_._2.nonEmpty)

      val r =
        if (t.isEmpty) {
          EmptyMDD
        } else {
          new MDDNode(t.toMap)
        }

      existing(k).getOrElseUpdate(r, r)
    }
  }

  override def loadCSPOM(args: List[String]) = {

    val (n, d, k, e, l, q, s) = params(args(0))

    val rand = new Random(s)

    CSPOM {
      val vars = List.fill(n)(IntVariable(0 until d))

      val r = new CoarseProportionRandomListGenerator(n, k, s);

      for (scope <- r.selectTuples(e, Structure.UNSTRUCTURED, false, false)) {
        val mdd = new LazyMDD(Unit => {
          val r = RandomMDD.apply(d, k, l, q, rand)
          //println(r.edges)
          r
        })
        ctr(table(mdd, false, scope map vars))
      }
    }
  }

  def description(args: List[String]) = {
    val (n, d, k, e, l, q, s) = params(args(0))
    s"mdd-$n-$d-$k-$e-$l-$q-$s"
  }

  def control(solution: Map[String, Any]) = ???

  def params(args: String) = {
    val Array(nbVariables, domainSize, arity, nbConstraints,
      looseness, mddProb, seed) = args.split(":")
    val n = nbVariables.toInt
    val d = domainSize.toInt
    val k = arity.toInt
    val e = nbConstraints.toInt
    val l = looseness.toDouble match {
      case l if l > 0 => l
      case l => math.exp(-n * math.log(d) / e)
    }
    val q = mddProb.toDouble
    val s = seed.toLong

    (n, d, k, e, l, q, s)
  }

  run(args)
}

object TestMDD extends App {
  val Array(domainSize, arity, looseness, mddProb, iterations) = args(0).split(":")

  val d = domainSize.toInt
  val k = arity.toInt
  val l = looseness.toDouble
  val q = mddProb.toDouble
  var lambda: List[BigInt] = Nil
  var nu: List[Long] = Nil

  for (seed <- 0 until iterations.toInt) {
    print(s"$seed : ")
    val rand = new Random(seed)
    val m = RandomMDD(d, k, l, q, rand)
    lambda ::= m.lambda
    nu ::= m.edges.toLong
    println(s"${m.lambda} tuples in ${m.edges} edges")
  }
  val lambdaAvg = StatisticsManager.average(lambda)
  val lambdaStDev = StatisticsManager.stDev(lambda)

  val nuAvg = StatisticsManager.average(nu)
  val nuStDev = StatisticsManager.stDev(nu)

  println(s"$lambdaAvg - $lambdaStDev - ${lambdaAvg + 2.3 * lambdaStDev}")
  println(s"$nuAvg - $nuStDev - ${nuAvg + 2.3 * nuStDev}")
}

object NameMDD extends App {
  SQLWriter.connection(new URI("postgresql://concrete:Wizcof25@raihmsvg")).withSession {
    val f = io.Source.fromFile(args(0))
    val setP = Q.update[(String, Int, Int, String)]("""UPDATE Problems
      SET display =?, nbVars=?, nbCons =?
      WHERE name =?""")

    val tag = Q.update[(String, String)]("""
        INSERT INTO ProblemTags 
        SELECT ?, problemId 
        FROM Problems NATURAL LEFT JOIN ProblemTags
        WHERE name = ? AND tag IS NULL""")

    for (line <- f.getLines if !line.startsWith("#")) {
      for (sp <- 0 until 25) {
        val (n, d, k, e, l, q, s) = RandomMDD.params(s"$line:$sp")
        val lp = f"${100 * l}%.0f"
        val lq = f"${100 * q}%.0f"
        val name = RandomMDD.description(List(s"$line:$sp"))
        val display = s"mdd-$n-$d-$k-$e-$lp-$lq-$s"
        println(name)
        setP.execute((display, n, e, name))

        tag.execute((s"mdd-$n-$d-$k-$e-$lp-$lq", name))
      }

    }

  }
}

object RenameMDD extends App {
  SQLWriter.connection(new URI("postgresql://concrete:Wizcof25@raihmsvg")).withSession {

    val r = sql"""SELECT problemId, name FROM Problems WHERE name ~ '^mdd-'""".as[(Int, String)]

    val setN = Q.update[(String, Int)]("""UPDATE Problems SET name = ? WHERE problemId = ?""")

    for (list <- r) {
      val (id, name) = list
      val newName = RandomMDD.description(List(name.split("-").tail.mkString(":")))
      println(newName)
      setN.execute((newName, id))
    }

    //    val setP = Q.update[(String, Int, Int, String)]("""UPDATE Problems
    //      SET display =?, nbVars=?, nbCons =?
    //      WHERE name =?""")
    //
    //    val tag = Q.update[(String, String)]("""
    //        INSERT INTO ProblemTags 
    //        SELECT ?, problemId 
    //        FROM Problems NATURAL LEFT JOIN ProblemTags
    //        WHERE name ~ ? AND tag IS NULL""")
    //
    //    for (line <- f.getLines if !line.startsWith("#")) {
    //      val Array(n, d, k, e, l, q) = line.split(":")
    //      val lp = f"${100 * l.toDouble}%.0f"
    //      val lq = f"${100 * q.toDouble}%.0f"
    //      println(s"mdd-$n-$d-$k-$e-$l-$q")
    //      for (s <- 0 until 25) {
    //        setP.execute((s"mdd-$n-$d-$k-$e-$lp-$lq-$s",
    //          n.toInt, e.toInt, s"mdd-$n-$d-$k-$e-$l-$q-$s"))
    //      }
    //
    //      tag.execute((s"mdd-$n-$d-$k-$e-$lp-$lq", s"^mdd-$n-$d-$k-$e-$l-$q-"))))))))
    //
    //    }
    //
  }
}
