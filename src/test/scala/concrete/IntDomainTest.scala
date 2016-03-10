package concrete

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen

final class IntDomainTest extends FlatSpec with Matchers with PropertyChecks {

  "IntDomain" should "find next/prev" in {
    val b = IntDomain.ofSeq(1, 2, 7, 8)
    b.prev(3) shouldBe 2
    b.prev(2) shouldBe 1
    b.next(2) shouldBe 7
    b.prev(9) shouldBe 8
    a[NoSuchElementException] should be thrownBy b.next(9)
    a[NoSuchElementException] should be thrownBy b.prev(1)
    a[NoSuchElementException] should be thrownBy b.prev(0)
  }

  it should "behave as sets" in {
    forAll(Gen.containerOf[Set, Int](Gen.choose(-10000, 10000))) { s =>
      val s2 = s.foldLeft[IntDomain](EmptyIntDomain)(_ | _)
      s2 should contain theSameElementsAs s
    }
    forAll(Gen.containerOf[Set, Int](Gen.choose(0, 63))) { s =>
      val s2 = s.foldLeft[IntDomain](EmptyIntDomain)(_ | _)
      s2 should contain theSameElementsAs s
    }
  }

  it should "enumerate" in {
    IntDomain.ofSeq(1, 2, 7, 8) should contain theSameElementsAs Seq(1, 2, 7, 8)
  }

  it should "detect presence" in {
    val domain = IntDomain.ofSeq(0, 1)
    assert(domain.present(0))

    val d2 = domain.remove(0);
    assert(!d2.present(0));

    assert(domain.present(0));
  }

  it should "split" in {
    val domain = IntDomain.ofSeq(3, 4, 5, 7)
    domain.removeTo(4).head shouldBe 5
    domain.removeFrom(6).last shouldBe 5
    domain.removeFrom(0) shouldBe empty
    domain.removeFrom(3) shouldBe empty
    domain.removeTo(7) shouldBe empty
    domain.removeTo(8) shouldBe empty
    domain.removeFrom(-1) shouldBe empty
    domain.removeUntil(7) should contain theSameElementsAs Seq(7)
    domain.removeUntil(400) shouldBe empty
    domain.removeFrom(400) shouldBe domain
  }

  it should "convert bit vector to interval" in {
    IntDomain.ofSeq(5, 6, 7) shouldBe an[IntervalDomain]
  }

  it should "compute unions from intervals" in {
    val d1 = IntDomain.ofInterval(5, 10)
    d1 should contain theSameElementsAs (5 to 10)

    val u1 = d1 | IntDomain.ofSeq(12, 15)

    u1 should contain theSameElementsAs (5 to 10) ++ Seq(12, 15)
    u1 shouldBe a[BitVectorDomain]

    val u2 = d1 | IntDomain.ofInterval(12, 15)
    u2 should contain theSameElementsAs (5 to 10) ++ (12 to 15)
    u2 shouldBe a[BitVectorDomain]

    val u3 = d1 | IntDomain.ofInterval(10, 15)
    u3 should contain theSameElementsAs (5 to 15)
    u3 shouldBe an[IntervalDomain]

    val u4 = d1 | IntDomain.ofInterval(8, 10)
    u4 should contain theSameElementsAs d1
    u4 shouldBe an[IntervalDomain]

    val u5 = d1 | IntDomain.ofInterval(11, 15)
    u5 should contain theSameElementsAs (5 to 15)
    u5 shouldBe an[IntervalDomain]

    val u6 = d1 | Singleton(15)
    u6 should contain theSameElementsAs (5 to 10) ++ Seq(15)
    u6 shouldBe a[BitVectorDomain]

    val u7 = d1 | Singleton(11)
    u7 should contain theSameElementsAs (5 to 11)
    u7 shouldBe an[IntervalDomain]

    d1 | Singleton(10) should be theSameInstanceAs d1

    d1 | EmptyIntDomain should be theSameInstanceAs d1

  }

  it should "compute intersections from intervals" in {
    val d1 = IntDomain.ofInterval(5, 10)

    d1 & IntDomain.ofSeq(7, 15) shouldBe Singleton(7)

    d1 & IntDomain.ofSeq(12, 15) shouldBe EmptyIntDomain

    d1 & IntDomain.ofInterval(12, 15) shouldBe EmptyIntDomain

    d1 & IntDomain.ofInterval(10, 15) shouldBe Singleton(10)

    val i1 = d1 & IntDomain.ofInterval(7, 15)
    i1 should contain theSameElementsAs (7 to 10)
    i1 shouldBe an[IntervalDomain]

    d1 & IntDomain.ofInterval(0, 10) shouldBe d1

    d1 & IntDomain.ofSeq(3, 5, 6, 7, 8, 9, 10) shouldBe d1
  }

  it should "compute unions from bit vectors" in {
    val d1 = IntDomain.ofSeq(5, 7, 10)

    val u1 = d1 | IntDomain.ofSeq(12, 15)

    u1 should contain theSameElementsAs Seq(5, 7, 10) ++ Seq(12, 15)
    u1 shouldBe a[BitVectorDomain]

    val u2 = d1 | IntDomain.ofInterval(10, 15)
    u2 should contain theSameElementsAs Seq(5, 7) ++ (10 to 15)
    u2 shouldBe a[BitVectorDomain]

    val u3 = d1 | IntDomain.ofInterval(5, 15)
    u3 should contain theSameElementsAs (5 to 15)
    u3 shouldBe an[IntervalDomain]

    val u6 = d1 | Singleton(15)
    u6 should contain theSameElementsAs Seq(5, 7, 10, 15)
    u6 shouldBe a[BitVectorDomain]

    d1 | Singleton(10) should be theSameInstanceAs d1

    d1 | EmptyIntDomain should be theSameInstanceAs d1

  }

  it should "compute intersections from bit vectors" in {
    val d1 = IntDomain.ofSeq(5, 7, 10)

    d1 & IntDomain.ofSeq(7, 15) shouldBe Singleton(7)

    d1 & IntDomain.ofSeq(12, 15) shouldBe EmptyIntDomain

    d1 & IntDomain.ofInterval(12, 15) shouldBe EmptyIntDomain

    d1 & IntDomain.ofInterval(10, 15) shouldBe Singleton(10)

    val i1 = d1 & IntDomain.ofInterval(7, 15)
    i1 should contain theSameElementsAs Seq(7, 10)
    i1 shouldBe a[BitVectorDomain]

    d1 & IntDomain.ofInterval(0, 10) shouldBe d1

    d1 & IntDomain.ofSeq(5, 7, 10) shouldBe d1
  }

  it should "compute unions from singletons" in {
    val d1 = Singleton(10)

    d1 | Singleton(10) should be theSameInstanceAs d1
    d1 | EmptyIntDomain should be theSameInstanceAs d1
    val u1 = d1 | Singleton(11)
    u1 should contain theSameElementsAs (10 to 11)
    u1 shouldBe an[IntervalDomain]

    val u2 = d1 | Singleton(12)
    u2 should contain theSameElementsAs Seq(10, 12)
    u2 shouldBe a[BitVectorDomain]
  }
}