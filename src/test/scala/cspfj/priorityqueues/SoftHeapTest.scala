package cspfj.priorityqueues;

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import java.util.Queue
import org.junit.Test
import cspfj.util.Loggable
import scala.util.Random

final class SoftHeapTest extends Loggable {

  val RANDOM = new Random()

  private val INTS = Stream.continually(IntNode(RANDOM.nextInt(5000000))).take(100000)

  @Test
  def test() {
    val maximier = new SoftHeap[IntNode](
      IntNode.key);

    assertEquals(maximier.size, 0);
    assertTrue(maximier.isEmpty);

    INTS.foreach(maximier.offer)

    assertEquals(maximier.size, INTS.length);

    var errors = 0;
    var last = maximier.poll().v;
    while (!maximier.isEmpty()) {
      val current = maximier.poll().v;
      // System.out.println(current);
      // assertTrue(current + " should be >= " + last, current >= last);
      if (current < last) {
        errors += 1;
      }
      last = current;

    }
    info(errors + " errors (rate = " + errors.toDouble / INTS.length + ")");

  }

}
