package cspfj.priorityqueues

import java.util.Queue

import scala.util.Random

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

case class IntNode(val v: Int) extends BinomialHeapNode[IntNode]

final class ScalaPriorityQueueTest {

  private val RANDOM = new Random(0);

  private val INTS = Stream.continually(IntNode(RANDOM.nextInt(5000000))).take(100000)

  private val key = new Key[IntNode]() {
    def getKey(o: IntNode) = o.v
  }

  @Test
  def testScalaBinomialHeap() {
    test(new ScalaBinomialHeap[IntNode](key))
  }

  @Test
  def testScalaNative() {
    test(new ScalaNative[IntNode](key))
  }

  @Test
  def testJavaNative() {
    test(new JavaNative[IntNode](key))
  }
  @Test
  def testSkewHeap() {
    test(new SkewHeap[IntNode](key))
  }

  def test(maximier: Queue[IntNode]) {

    assertEquals(maximier.size, 0);
    assertTrue(maximier.isEmpty);

    INTS.foreach { i => assertFalse(i.isPresent); maximier.offer(i) }

    assertEquals(maximier.size, INTS.length);

    var last = maximier.poll().v;
    while (!maximier.isEmpty) {
      val current = maximier.poll().v;
      assertTrue(current + " should be >= " + last, current >= last);
      last = current;
    }

  }

}
