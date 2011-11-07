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

  private var headNode = new Node[A](null, null)
  headNode.prev = headNode
  headNode.next = headNode

  def update(idx: Int, elem: A) {
    var count = idx
    var current = headNode.next
    while (count > 0) {
      count -= 1
      current = current.next
    }
    current.asInstanceOf[ContentNode[A]].item = elem
  }

  def apply(idx: Int) = {
    iterator.drop(idx).next
  }

  def length = iterator.length

  def merge(list: DLList[A]) {
    if (isEmpty) {
      headNode = list.headNode
    } else if (!list.isEmpty) {
      list.headNode.next.prev = headNode.prev
      headNode.prev.next = list.headNode.next
      
      list.headNode.prev.next = headNode
      headNode.prev = list.headNode.prev
    }
  }

  override def isEmpty = headNode.next == headNode

  def add(elem: A) {
    val newNode = new ContentNode(elem, headNode.prev, headNode)
    headNode.prev.next = newNode
    headNode.prev = newNode
  }

  def addNode(newNode: ContentNode[A]) {
    newNode.prev = headNode.prev
    newNode.next = headNode
    headNode.prev.next = newNode
    headNode.prev = newNode
  }

  override def head = {
    val firstNode = headNode.next
    if (firstNode == headNode) throw new NoSuchElementException
    firstNode.asInstanceOf[ContentNode[A]].item
  }

  override def companion: GenericCompanion[DLList] = DLList

  def mutableIterator = new DLLIterator()

  override def iterator = new DLLIterator()

  class DLLIterator extends Iterator[A] {

    var current = headNode.next
    var lastReturned: ContentNode[A] = null
    def hasNext = current != headNode
    def next() = {
      lastReturned = current.asInstanceOf[ContentNode[A]]
      current = current.next
      lastReturned.item
    }

    def remove() = {
      lastReturned.prev.next = lastReturned.next
      lastReturned.next.prev = lastReturned.prev
      lastReturned
    }
  }
  
  override def reverseIterator = new Iterator[A] {
    var current = headNode.prev
    def hasNext = current != headNode
    def next() = {
      val elem = current.asInstanceOf[ContentNode[A]].item
      current = current.prev
      elem
    }
  }

}

class Node[A](
  var prev: Node[A],
  var next: Node[A]);

class ContentNode[A](var item: A, prev: Node[A], next: Node[A])
  extends Node[A](prev, next)

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
