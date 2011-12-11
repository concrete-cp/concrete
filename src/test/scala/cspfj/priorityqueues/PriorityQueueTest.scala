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
  val fhkey = new Key[FHNode]() {
    def getKey(o: FHNode) = o.v
  }
}

case class IntNode(val v: Int) extends Identified with BinomialHeapNode[IntNode] {
  val getId = IntNode.id
  IntNode.id += 1
}

case class FHNode(val v: Int) extends FibonacciHeapNode[FHNode]

final class PriorityQueueTest {

  private val RANDOM = new Random(0);

  private val INTS = Stream.continually(IntNode(RANDOM.nextInt(5000000))).take(100000)

  private val FHS = Stream.continually(FHNode(RANDOM.nextInt(5000000))).take(7000)

  val key = IntNode.key

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
  @Test
  def testBinaryHeap() {
    test(new BinaryHeap[IntNode](key))
  }
  @Test
  def testBinomialHeap() {
    test(new BinomialHeap[IntNode](key))
  }
  @Test
  def testFibonacciHeap() {
    test(new FibonacciHeap[IntNode](key))
  }
  @Test
  def testScalaFibonacciHeap() {
    val maximier = new ScalaFibonacciHeap[FHNode](IntNode.fhkey)
    assertEquals(maximier.size, 0);
    assertTrue(maximier.isEmpty);

    FHS.foreach { i => assertFalse(i.isPresent); assertTrue(maximier.offer(i)) }

    assertEquals(maximier.size, FHS.length);

    var last = maximier.poll().v;
    while (!maximier.isEmpty) {
      val current = maximier.poll().v;
      assertTrue(current + " should be >= " + last, current >= last);
      last = current;
    }

  }

  def test(maximier: Queue[IntNode]) {
    //for (j <- 3001 to 5000) {
    val j = INTS.size
    maximier.clear()

    assertEquals(maximier.size, 0);
    assertTrue(maximier.isEmpty);

    INTS.take(j).foreach { i => assertFalse(i.isPresent); assertTrue(maximier.offer(i)) }

    assertEquals(j, maximier.size);

    println(j)
    var last = maximier.poll().v;
    while (!maximier.isEmpty) {
      val current = maximier.poll().v;
      assertTrue(current + " should be >= " + last, current >= last);
      last = current;
    }
    //}

  }

}
