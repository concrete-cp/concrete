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

object RandomMDD extends Concrete with App {

  def apply(d: Int, k: Int, l: Double, q: Double, s: Int) = {
    val rand = new Random(s)
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
      val r = new RandomMDDNode(
        Array.fill(d)(generate(d, k - 1, l, q, existing, rand)))
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
    val s = seed.toInt

    val cp = new CSPOM()
    val vars = List.fill(n)(cp.interVar(0, d - 1))

    val r = new CoarseProportionRandomListGenerator(n, k, s);

    for (scope <- r.selectTuples(e, Structure.UNSTRUCTURED, false, false)) {
      val mdd = RandomMDD(d, k, l, q, s)
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

abstract class RandomMDD extends Relation {
  def contains(t: Seq[_]) = throw new UnsupportedOperationException

  def close() {

  }

  def literator: Iterator[List[Int]]
  def iterator = literator map (_.toArray)

  def arity = k.get

  def k: Option[Int]
}

object EmptyRandomMDD extends RandomMDD {
  def literator = Iterator()
  def k = None
}

object RandomMDDLeaf extends RandomMDD {
  def literator = Iterator(Nil)
  def k = Some(0)
}

final class RandomMDDNode(var trie: Array[RandomMDD]) extends RandomMDD {

  override def close { trie = null }

  def literator = trie.iterator.zipWithIndex flatMap {
    case (t, i) => t.literator map (i :: _)
  }

  def k = trie.iterator.map(_.k).find(_.isDefined).map(_.get + 1)

}