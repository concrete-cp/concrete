package concrete.priorityqueues

import scala.util.Random

import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import org.scalatest.Matchers
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.Seconds
import org.scalatest.time.Span

object IntNode {
  var id: Int = 0
}

case class IntNode(val v: Int) extends Identified with PTag {
  val id = IntNode.id
  IntNode.id += 1
}

final class PriorityQueueTest extends FlatSpec with Matchers with Timeouts with Inspectors {

  private val RANDOM = new Random(0);

  private val INTS = Seq.fill(100000)(IntNode(RANDOM.nextInt(5000000)))

  "ScalaNative" should behave like test(new ScalaNative[IntNode]())

  "JavaNative" should behave like test(new JavaNative[IntNode]())

  "BinaryHeap" should behave like test(new BinaryHeap[IntNode]())

  "BinomialHeap" should behave like test(new BinomialHeap[IntNode]())

  "FibonacciHeap" should behave like test(new FibonacciHeap[IntNode]())

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
    failAfter(Span(5, Seconds)) {
      var j = 100
      while (j <= INTS.size) {

        q.clear()

        q shouldBe empty

        forAll(INTS.take(j)) { i => assert(q.offer(i, i.v)) }

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

        j = (j * 10).toInt
      }
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
