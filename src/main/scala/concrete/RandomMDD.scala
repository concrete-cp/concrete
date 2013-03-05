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
import rb.randomlists.RandomListGenerator.Structure
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

object RandomMDD extends Concrete with App {

  def apply(d: Int, k: Int, l: Double, q: Double, rand: Random) = {
    val existing = Array.fill(k + 1)(new ArrayBuffer[RandomMDD]())
    generate(d, k, l, q, existing, rand)
  }

  def generate(d: Int, k: Int, l: Double, q: Double,
    existing: Array[ArrayBuffer[RandomMDD]], rand: Random): RandomMDD = {
    if (k == 0) {
      if (rand.nextDouble < l) {
        RandomMDDLeaf
      } else {
        EmptyRandomMDD
      }
    } else if (existing(k).nonEmpty && rand.nextDouble < q) {
      existing(k)(rand.nextInt(existing(k).size))
    } else {
      val t = Array.fill(d)(generate(d, k - 1, l, q, existing, rand))

      val r =
        if (t.forall(_ eq EmptyRandomMDD)) {
          EmptyRandomMDD
        } else {
          new RandomMDDNode(t)
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
  var lambda: List[Int] = Nil
  var nu: List[Int] = Nil
  var ts = 0
  for (seed <- 0 until 10000) {
    val rand = new Random(seed)
    val m = RandomMDD(d, k, l, q, rand).reduce
    ts += 1
    println(ts)
    lambda ::= m.size
    nu ::= m.edges(ts)
  }
  println(StatisticsManager.average(lambda))
  println(StatisticsManager.average(nu))
}

abstract class RandomMDD extends Relation {
  def contains(t: Seq[_]) = throw new UnsupportedOperationException

  def close() {

  }
  def reduce: RandomMDD = {
    reduce(new HashMap())
  }
  def reduce(mdds: collection.mutable.Map[Seq[RandomMDD], RandomMDD]): RandomMDD
  def literator: Iterator[List[Int]]
  def iterator = literator map (_.toArray)

  def arity = k.get

  def k: Option[Int]

  def edges(ts: Int): Int
  override def toString = literator.mkString("\n")
  def toString(k: Int): String
}

object EmptyRandomMDD extends RandomMDD {
  def literator = Iterator()
  def k = None
  def edges(ts: Int) = 0
  def reduce(mdds: collection.mutable.Map[Seq[RandomMDD], RandomMDD]) = this
  def toString(k: Int) = ""
}

object RandomMDDLeaf extends RandomMDD {
  def literator = Iterator(Nil)
  def k = Some(0)
  def edges(ts: Int) = 0
  def reduce(mdds: collection.mutable.Map[Seq[RandomMDD], RandomMDD]) = this
  def toString(k: Int) = "\n"
}

final class RandomMDDNode(var trie: Array[RandomMDD]) extends RandomMDD {
  var timestamp: Int = _
  override def close { trie = null }

  def literator = trie.iterator.zipWithIndex flatMap {
    case (t, i) => t.literator map (i :: _)
  }

  def k = trie.iterator.map(_.k).find(_.isDefined).map(_.get + 1)

  def reduce(mdds: collection.mutable.Map[Seq[RandomMDD], RandomMDD]): RandomMDD = {
    var b = trie.map(_.reduce(mdds))
    mdds.getOrElseUpdate(b, new RandomMDDNode(b))
  }

  def edges(ts: Int) =
    if (ts == timestamp)
      0
    else {
      timestamp = ts
      trie.count(_ ne EmptyRandomMDD) + trie.map(_.edges(ts)).sum
    }

  override lazy val hashCode: Int = {
    //val hash = new MurmurHash3
    MurmurHash3.arrayHash(trie)
  }

  override def equals(o: Any): Boolean = o match {
    case t: RandomMDDNode =>
      val len = t.trie.length
      len == trie.length && {
        var i = len - 1
        while (i >= 0 && (t.trie(i) eq trie(i))) {
          i -= 1
        }
        i < 0
      }
    case _ => false
  }

  def toString(k: Int) = {
    val space = (0 until k).map(" ").mkString("")
    trie.zipWithIndex.filter(_._1 ne EmptyRandomMDD).map {
      case (t, i) => space + i + "\n" + t.toString(k + 1)
    } mkString ("")

  }
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