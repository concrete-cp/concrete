package cspfj.priorityqueues

import java.util.Queue

import scala.util.Random

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

object IntNode {
  var id: Int = 0
  val key = new Key[IntNode]() {
    def getKey(o: IntNode) = o.v
  }

}

case class IntNode(val v: Int) extends Identified with PTag with LazyKey[IntNode] {
  val getId = IntNode.id
  IntNode.id += 1
}

final class PriorityQueueTest {

  private val RANDOM = new Random(0);

  private val INTS = Stream.continually(IntNode(RANDOM.nextInt(5000000))).take(100000)

  val key = IntNode.key

  //  @Test //(timeout = 5000)
  //  def testScalaBinomialHeap() {
  //    test(new ScalaBinomialHeap[IntNode](key))
  //  }

  @Test(timeout = 5000)
  def testScalaNative() {
    test(new ScalaNative[IntNode](key))
  }

  @Test(timeout = 5000)
  def testJavaNative() {
    test(new JavaNative[IntNode](key))
  }
  @Test(timeout = 5000)
  def testSkewHeap() {
    test(new SkewHeap[IntNode](key))
  }
  @Test(timeout = 5000)
  def testBinaryHeap() {
    test(new BinaryHeap[IntNode](key))
  }
  @Test(timeout = 5000)
  def testBinomialHeap() {
    test(new BinomialHeap[IntNode](key))
  }
  @Test(timeout = 20000)
  def testFibonacciHeap() {
    test(new FibonacciHeap[IntNode](key))
  }
  //  @Test(timeout = 5000)
  //  def testScalaFibonacciHeap() {
  //    test(new ScalaFibonacciHeap[IntNode](key))
  //  }
//  @Test(timeout = 5000)
//  def testScalaIOBinomialHeap() {
//    test(new ScalaIOBinomialHeap[IntNode](key))
//  }

  def test(q: Queue[IntNode]) {
    //for (j <- 3001 to 5000) {
    //INTS.foreach(i => i.unsetPresent())
    var j = 100
    while (j <= INTS.size) {

      q.clear()

      assertEquals(q.size, 0);
      assertTrue(q.isEmpty);

      INTS.take(j).foreach { i => assertTrue(q.offer(i)) }

      assertEquals(j, q.size);
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

}
