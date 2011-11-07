package cspfj.util
import org.junit.Test
import org.junit.Assert._

class DLListTest {

  @Test
  def testCreate {
    val l = new DLList[Int]
    assertEquals(Seq(), l)
    l.add(3)
    l.add(5)
    assertEquals(Seq(3, 5), l)
    assertEquals(Seq(5, 3), l.reverseIterator.toSeq)
  }

  @Test
  def testMerge {
    val l0 = DLList(3, 5, 8)
    val l1 = DLList(2, 3, 9)
    l0.merge(l1)
    assertEquals(Seq(3, 5, 8, 2, 3, 9), l0)
    assertEquals(Seq(9, 3, 2, 8, 5, 3), l0.reverseIterator.toSeq)
  }

  @Test
  def testRemove {
    val l = DLList(1, 2, 3, 4, 5)
    val itr = l.mutableIterator
    assertEquals(1, itr.next)
    assertEquals(1, itr.remove.item)
    assertEquals(Seq(2, 3, 4, 5), l)
    assertEquals(2, itr.next)
    assertEquals(3, itr.next)
    assertEquals(3, itr.remove.item)
    assertEquals(Seq(2, 4, 5), l)
    assertEquals(4, itr.next)
    assertEquals(5, itr.next)
    assertFalse(itr.hasNext)
    assertEquals(5, itr.remove.item)
    assertFalse(itr.hasNext)
    assertEquals(Seq(2, 4), l)

    val itr2 = l.mutableIterator
    assertEquals(2, itr2.next)
    assertEquals(2, itr2.remove.item)
    assertEquals(Seq(4), l)
    assertEquals(4, itr2.next)
    assertEquals(4, itr2.remove.item)
    assertEquals(Seq(), l)
  }

}