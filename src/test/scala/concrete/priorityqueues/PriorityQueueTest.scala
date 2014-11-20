package concrete.priorityqueues

import java.util.Queue

import scala.util.Random

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

object IntNode {
  var id: Int = 0
}

case class IntNode(val v: Int) extends Identified with PTag {
  val id = IntNode.id
  IntNode.id += 1
}

final class PriorityQueueTest {

  private val RANDOM = new Random(0);

  private val INTS = Seq.fill(100000)(IntNode(RANDOM.nextInt(5000000)))

  //  @Test //(timeout = 5000)
  //  def testScalaBinomialHeap() {
  //    test(new ScalaBinomialHeap[IntNode](key))
  //  }

  @Test(timeout = 5000)
  def testScalaNative() {
    test(new ScalaNative[IntNode]())
  }

  @Test(timeout = 5000)
  def testJavaNative() {
    test(new JavaNative[IntNode]())
  }
  @Test //(timeout = 5000)
  def testBinaryHeap() {
    test(new BinaryHeap[IntNode]())
  }
  @Test(timeout = 5000)
  def testBinomialHeap() {
    test(new BinomialHeap[IntNode]())
  }
  @Test(timeout = 20000)
  def testFibonacciHeap() {
    test(new FibonacciHeap[IntNode]())
  }
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
    var j = 100
    while (j <= INTS.size) {

      q.clear()

      assertTrue(q.isEmpty);

      INTS.take(j).foreach { i => assertTrue(q.offer(i, i.v)) }

      //assertEquals(j, q.size);
      //println(j)
      var i = 1
      var last = q.poll().v;
      while (!q.isEmpty) {
        val current = q.poll().v;
        i += 1
        assertTrue(current + " should be >= " + last + " (j = " + j + ")", current >= last);
        last = current;
      }

      j = (j * 10).toInt
    }
    //}

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
