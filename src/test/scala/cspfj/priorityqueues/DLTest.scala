package cspfj.priorityqueues
import org.junit.Test
import org.junit.Assert._

class DLTest {

  class IntDLL(val v: Int) extends DLLNode[IntDLL]

  @Test
  def test {
    val l = new IntDLL(0)
    (1 to 100) foreach (i => l.add(new IntDLL(i)))

    var x = l.left
    var i = 100
    while (x != l) {
      assertEquals(i, x.v)

      x = x.left
      i -= 1
    }

    do {
      assertEquals(i, x.v)
      x = x.right
      i += 1
    } while (x != l)

    l.remove()
    assertEquals(l.right, l.left.right)
    assertEquals(l.left, l.right.left)

  }

}