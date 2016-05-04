package concrete.constraint.extension

import scala.BigInt
import scala.math.BigInt.int2bigInt
import scala.util.Random

import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import org.scalatest.Matchers

import concrete.Domain
import concrete.util.MDDGenerator
import concrete.util.SetWithMax
import cspom.extension.IdMap
import concrete.IntDomain
import concrete.Singleton

final class BDDTest extends FlatSpec with Matchers with Inspectors {

  private val ts: BDD = BDD(Seq(List(0, 0), List(0, 1), List(1, 0)))

  val t = BDD0 + List(1, 2, 3) + List(1, 3, 4) + List(1, 2, 5) + List(2, 3, 5)
  val s = BDD0 + List(1, 2, 5) + List(1, 3, 4) + List(1, 2, 3) + List(2, 3, 5)

  val u = BDD(Seq(
    List(1, 2, 3),
    List(1, 3, 4),
    List(1, 2, 5),
    List(2, 3, 5)))

  "MDD" should "detect containment" in {
    ts should contain(Array(0, 1))
    ts should not contain (Array(1, 1))

    t should contain(Array(1, 3, 4))
    t should not contain (Array(1, 2, 4))

    forAll(t.toSeq.map(_.toArray)) { tuple =>
      s should contain(tuple)
      u should contain(tuple)
    }
  }

  it should "iterate over all tuples" in {
    ts.iterator.size shouldBe ts.lambda
  }

  it should "compute its size correctly" in {
    ts.lambda shouldBe BigInt(3)

    t.lambda shouldBe s.lambda

    u.lambda shouldBe t.lambda
  }

  it should "reduce" in {
    val m0 = BDD(Seq(
      List(2, 3, 2),
      List(1, 2, 1),
      List(1, 1, 1),
      List(1, 1, 3),
      List(3, 1, 1),
      List(3, 1, 3)))

    println(m0.edges(6))

    m0.lambda shouldBe BigInt(6)

    val m = m0.reduce()

    withClue(m) {
      m.lambda shouldBe BigInt(6)
      m.edges(1) should be <= 11
    }

  }

  def mddl(d: Int, k: Int, i: Int = 0): BDD = {
    if (i >= d) { BDD0 }
    else if (k <= 0) { BDDLeaf }
    else {
      new BDDNode(i, mddl(d, k - 1), mddl(d, k, i + 1))
    }
  }

  def mdd(d: Int, k: Int): MDD = {
    if (k <= 0) {
      MDDLeaf
    } else {
      MDD((0 until d).map(i => i -> mdd(d, k - 1)).toMap)
    }
  }

  it should "reduce quickly" in {
    val d = 6
    val k = 7

    val m1 = mddl(d, k)

    m1.lambda shouldBe BigInt(d).pow(k)
    m1.edges(1) shouldBe (1 - BigInt(d).pow(k + 1)) / (1 - d) - 1

    val t = System.nanoTime()
    val m = m1.reduce()
    val e = System.nanoTime()

    //println((e - t) / 1e9)

    m.lambda shouldBe BigInt(d).pow(k)
    m.edges(2) shouldBe d * k

    val m2 = mdd(d, k)
    m2.lambda shouldBe BigInt(d).pow(k)
    m2.edges(1) shouldBe (1 - BigInt(d).pow(k + 1)) / (1 - d) - 1

    val t2 = System.nanoTime()
    val m3 = m2.reduce()
    val e2 = System.nanoTime()

    //println((e2 - t2) / 1e9)

    m3.lambda shouldBe BigInt(d).pow(k)
    m3.edges(2) shouldBe d * k

  }

  it should "filter" in {
    val m = BDD(Seq(
      List(2, 3, 2),
      List(1, 2, 1),
      List(1, 1, 1),
      List(1, 1, 3),
      List(3, 1, 1),
      List(3, 1, 3)))
      .reduce()

    val l1 = new SetWithMax(3)
    m.fillFound(1, { case t => println(t); false }, 0, l1)

    //println(l1)

    val doms = Array[Domain](
      IntDomain(1 to 3),
      Singleton(1),
      IntDomain(1 to 3))

    val n = m.filterTrie(2, doms, List(1), 0)

    //println(n)

    val l = new SetWithMax(3)
    val sup = Array.fill(3)(Set[Int]())
    n.fillFound(3, {
      case (p, i) =>
        sup(p) += i; sup(p).size == 3
    }, 0, l)

    //println(l)

  }

  it should "be smaller than array-based MDD" in {
    val rand = new Random(0)
    val mdd = MDDGenerator(15, 5, 600000, rand)
    val mddf = mdd.reduce()
    val mddl = BDD(mdd.map(_.toList)).reduce()
    val ef = mddf.edges(5)
    val el = mddl.edges(5)
    ef should be >= el

  }

  it should "filter the same as MDD" in {
    val rand = new Random(0)
    val d = 5
    val k = 5
    val l = .28
    val lambda = (l * math.pow(d, k)).toInt
    var mddr = MDDGenerator(d, k, lambda, rand).reduce
    var mddl = BDD(mddr)

    mddr.edges(-2) shouldBe mddl.edges(-1)

    val doms = Array.fill[Domain](k)(IntDomain(0 until d))

    var ts = 1
    while (mddr.nonEmpty && mddl.nonEmpty) {
      mddr.lambda shouldBe mddl.lambda
      mddr.edges(ts) should be >= mddl.edges(ts)
      mddr should contain theSameElementsAs mddl
      ts += 1
      var mod = List.tabulate(k)(i => i).filter(_ => rand.nextDouble() < .5)
      for (pos <- mod) {
        var newd = doms(pos)
        while (newd eq doms(pos)) {
          newd = doms(pos).filter(_ => rand.nextDouble() > .1)
        }
        doms(pos) = newd
      }

      mddr = mddr.filterTrie(ts, doms, mod, 0)
      mddl = mddl.filterTrie(ts, doms, mod, 0)
      ts += 1
    }

    assert(mddr.isEmpty)
    assert(mddl.isEmpty)
  }

  it should "convert MDD to BDD" in {
    val m = MDD(Seq(
      Seq(2, 3, 2),
      Seq(1, 2, 1),
      Seq(1, 1, 1),
      Seq(1, 1, 3),
      Seq(3, 1, 1),
      Seq(3, 1, 3)))
      .reduce()

    val ml = BDD(m)

    m.edges(5) shouldBe ml.edges(10)
    m.toSet shouldBe ml.toSet

    val mlr = ml.reduce()
    mlr.edges(15) should be <= ml.edges(20)
  }

  it should "have correct number of nodes" in {
    val m = BDD(Seq(
      List(2, 3, 2),
      List(1, 2, 1),
      List(1, 1, 1),
      List(1, 1, 3),
      List(3, 1, 1),
      List(3, 1, 3)))
      .reduce()

    val map = m.nodes(new IdMap())

    map.size shouldBe 11

    val m2 = m.filterTrie(5, Array(IntDomain(1 to 3), Singleton(1), IntDomain(1 to 3)), List(1), 0)

    println(m2.toList)

    m2.nodes(map).size shouldBe 13

  }
}
