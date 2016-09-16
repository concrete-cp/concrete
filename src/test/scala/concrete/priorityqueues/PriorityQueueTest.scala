package concrete.priorityqueues

import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import org.scalatest.Matchers
import org.scalatest.concurrent.TimeLimits
import org.scalatest.prop.PropertyChecks

object IntNode {
  var id: Int = 0
}

case class IntNode(val v: Int) extends Identified with PTag {
  val id = IntNode.id
  IntNode.id += 1
}

final class PriorityQueueTest extends FlatSpec with Matchers with TimeLimits with PropertyChecks {

  //  private val RANDOM = new Random(0);
  //
  //  private val INTS = Seq.fill(100000)(IntNode(RANDOM.nextInt(5000000)))

  "ScalaNative" should "be correctly ordered" in test(new ScalaNative[IntNode]())

  "JavaNative" should "be correctly ordered" in test(new JavaNative[IntNode]())

  "BinaryHeap" should "be correctly ordered" in test(new BinaryHeap[IntNode]())

  "BinomialHeap" should "be correctly ordered" in test(new BinomialHeap[IntNode]())

  "FibonacciHeap" should "be correctly ordered" in test(new FibonacciHeap[IntNode]())

  //  @Test(timeout = 5000)
  //  def testScalaFibonacciHeap() {
  //    test(new ScalaFibonacciHeap[IntNode](key))
  //  }
  //  @Test(timeout = 5000)
  //  def testScalaIOBinomialHeap() {
  //    test(new ScalaIOBinomialHeap[IntNode](key))
  //  }

  def test(q: PriorityQueue[IntNode]) {
    //for (j <- 3001 to 5000) {
    //INTS.foreach(i => i.unsetPresent())
    forAll { data: Seq[Int] =>
      whenever(data.nonEmpty) {
        //failAfter(Span(5, Seconds)) {

        q.clear()

        q shouldBe empty

        Inspectors.forAll(data) { i =>
          val n = IntNode(i)
          q.offer(n, n.v)
        }

        //assertEquals(j, q.size);
        //println(j)
        var i = 1
        var last = q.poll().v;
        while (!q.isEmpty) {
          val current = q.poll().v;
          i += 1
          current should be >= last
          last = current;
        }

        i shouldBe data.size

      }
      //}
      //}
    }

  }

  //  @Test
  //  def testJavaFifos() {
  //    val q = new JavaFifos(IntNode.key, 1);
  //    for (e <- INTS) q.offer(e)
  //
  //    var i = INTS
  //    while (!q.isEmpty()) {
  //      val e = q.poll();
  //      assertEquals(e, i.head)
  //      i = i.tail
  //    }
  //  }

}
