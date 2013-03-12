package concrete

import java.io.ByteArrayInputStream
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import cspfj.generator.ProblemGenerator
import cspom.CSPOM
import cspom.Loggable
import cspom.extension.ExtensionConstraint
import cspom.extension.Relation
import rb.randomlists.CoarseProportionRandomListGenerator
import rb.randomlists.Structure
import scala.slick.session.Database
import scala.slick.jdbc.{ GetResult, StaticQuery => Q }
import Q.interpolation
import Database.threadLocalSession
import java.net.URI
import cspfj.StatisticsManager
import scala.collection.mutable.HashMap
import cspfj.constraint.extension.MDDn
import scala.util.hashing.MurmurHash3
import scala.collection.mutable.HashSet
import cspom.extension.EmptyMDD
import cspom.extension.MDDLeaf
import cspom.extension.MDDNode
import cspom.extension.MDD

object RandomMDD extends Concrete with App {

  def apply(d: Int, k: Int, l: Double, q: Double, rand: Random) = {
    val existing = Array.fill(k + 1)(new ArrayBuffer[MDD]())
    generate(d, k, l, q, existing, rand)
  }

  def generate(d: Int, k: Int, l: Double, q: Double,
    existing: Array[ArrayBuffer[MDD]], rand: Random): MDD = {
    if (k == 0) {
      if (rand.nextDouble < l) {
        MDDLeaf
      } else {
        EmptyMDD
      }
    } else if (existing(k).nonEmpty && rand.nextDouble < q) {
      existing(k)(rand.nextInt(existing(k).size))
    } else {
      val t = Map(
        (0 until d)
          map (i => i -> generate(d, k - 1, l, q, existing, rand))
          filter (_._2.nonEmpty): _*)

      val r =
        if (t.isEmpty) {
          EmptyMDD
        } else {
          new MDDNode(t)
        }
      existing(k) += r
      r
    }
  }

  var cProblem: Option[CSPOM] = None

  def load(args: List[String]) = {
    val Array(nbVariables, domainSize, arity, nbConstraints,
      looseness, mddProb, seed) = args(0).split(":")

    val n = nbVariables.toInt
    val d = domainSize.toInt
    val k = arity.toInt
    val e = nbConstraints.toInt
    val l = looseness.toDouble
    val q = mddProb.toDouble
    val rand = new Random(seed.toInt)

    val cp = new CSPOM()
    val vars = List.fill(n)(cp.interVar(0, d - 1))

    val r = new CoarseProportionRandomListGenerator(n, k, seed.toInt);

    for (scope <- r.selectTuples(e, Structure.UNSTRUCTURED, false, false)) {
      val mdd = RandomMDD(d, k, l, q, rand)
      // println(mdd.size)
      //      println(mdd map (_.mkString(",")) mkString ("\n"))
      //      exit
      cp.addConstraint(new ExtensionConstraint(mdd, false, scope map vars))
    }

    cProblem = Some(cp)
    val problem = ProblemGenerator.generate(cp)
    cp.closeRelations
    problem
  }

  def description(args: List[String]) = {
    val name = "mdd-" + args(0).split(":").mkString("-")
    (name, SQLWriter.md5(new ByteArrayInputStream(name.getBytes())))
  }

  def output(solution: Map[String, Int]) = {
    cProblem.get.variables.filter(!_.auxiliary).map(v =>
      solution.getOrElse(v.name, v.domain.values.head)).mkString(" ")
  }

  def control(solution: Map[String, Int]) = {
    cProblem.get.controlInt(solution) match {
      case s: Set[_] if s.isEmpty => None
      case s: Set[_] => Some(s.mkString(", "))
    }
  }

  run(args)
}

object TestMDD extends App {
  val Array(domainSize, arity, looseness, mddProb) = args(0).split(":")

  val d = domainSize.toInt
  val k = arity.toInt
  val l = looseness.toDouble
  val q = mddProb.toDouble
  var lambda: List[BigInt] = Nil
  var nu: List[Long] = Nil

  for (seed <- 0 until 10000) {
    print(seed + " ")
    val rand = new Random(seed)
    val m = RandomMDD(d, k, l, q, rand).reduce
    lambda ::= m.lambda
    nu ::= m.edges.toLong
  }
  println(StatisticsManager.average(lambda))
  println(StatisticsManager.average(nu))
}

object NameMDD extends App {
  SQLWriter.connection(new URI("postgresql://concrete:concrete@precision-vion")).withSession {
    val f = io.Source.fromFile(args(0))
    val setP = Q.update[(String, Int, Int, String)]("""UPDATE Problems
      SET display =?, nbVars=?, nbCons =?
      WHERE name =?""")

    val tag = Q.update[(String, String)]("""
        INSERT INTO ProblemTags 
        SELECT ?, problemId 
        FROM Problems NATURAL LEFT JOIN ProblemTags
        WHERE name ~ ? AND tag IS NULL""")

    for (line <- f.getLines if !line.startsWith("#")) {
      val Array(n, d, k, e, l, q) = line.split(":")
      val lp = f"${100 * l.toDouble}%.0f"
      val lq = f"${100 * q.toDouble}%.0f"
      println(s"mdd-$n-$d-$k-$e-$l-$q")
      for (s <- 0 until 10) {
        setP.execute((s"mdd-$n-$d-$k-$e-$lp-$lq-$s",
          n.toInt, e.toInt, s"mdd-$n-$d-$k-$e-$l-$q-$s"))
      }

      tag.execute((s"mdd-$n-$d-$k-$e-$lp-$lq", s"^mdd-$n-$d-$k-$e-$l-$q-"))

    }

  }
}