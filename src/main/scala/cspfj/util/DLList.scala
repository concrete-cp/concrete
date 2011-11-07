package cspfj.util
import scala.collection.generic.GenericTraversableTemplate
import scala.collection.mutable.LinearSeq
import scala.collection.mutable.SeqLike
import scala.collection.generic.GenericCompanion
import scala.collection.generic.SeqFactory
import scala.collection.mutable.Builder

class DLList[A]() extends LinearSeq[A]
  with GenericTraversableTemplate[A, DLList]
  with SeqLike[A, DLList[A]]
  with Serializable {

  var firstNode: Option[Node[A]] = None
  var lastNode: Option[Node[A]] = None

  def update(idx: Int, elem: A) {
    var count = idx
    var current = firstNode
    while (count > 0) {
      count -= 1
      current = current.get.next
    }
    current.get.content = elem
  }

  def apply(idx: Int) = {
    iterator.drop(idx).next
  }

  def length = iterator.length

  def append(list: DLList[A]) {
    if (isEmpty) {
      firstNode = list.firstNode
      lastNode = list.lastNode
    } else {
      lastNode.get.next = list.firstNode
      list.firstNode.get.prev = lastNode
      lastNode = list.lastNode
    }
  }
  
  override def isEmpty = firstNode == None

  def add(elem: A) {
    val newNode = Node(elem, None, firstNode)
    if (isEmpty) lastNode = Some(newNode)
    firstNode = Some(newNode)
  }

  override def head = firstNode match {
    case None => throw new NoSuchElementException
    case Some(elem) => elem.content
  }

  override def companion: GenericCompanion[DLList] = DLList

  override def iterator = new DLLIterator()

  class DLLIterator extends Iterator[A] {
    var current = firstNode
    def hasNext = current.isDefined
    def next() = {
      var elem = current.get.content
      current = current.get.next
      elem
    }
  }
}

case class Node[A](
  var content: A,
  var prev: Option[Node[A]] = None,
  var next: Option[Node[A]] = None);

object DLList extends SeqFactory[DLList] {
  def newBuilder[A]: Builder[A, DLList[A]] =
    new Builder[A, DLList[A]] {
      def emptyList() = new DLList[A]()
      var current = emptyList()

      def +=(elem: A): this.type = {
        current add elem
        this
      }

      def clear() { current = emptyList() }

      def result() = current
    }

}
