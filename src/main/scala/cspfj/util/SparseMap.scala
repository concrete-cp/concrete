package cspfj.util

import scala.util.hashing.MurmurHash3
import java.util.Arrays

class SparseMap[A](
  private val dense: Array[Int],
  private val sparse: Array[Int],
  private val map: Array[A],
  private val members: Int) extends Iterable[(Int, A)] {

  def this()(implicit manifest: Manifest[A]) = {
    this(new Array[Int](10), new Array[Int](10), new Array[A](10), 0)
  }

  def apply(k: Int): A = {
    val a = sparse(k)
    if (a < members && dense(a) == k) {
      map(k)
    } else {
      throw new NoSuchElementException
    }
  }

  def get(k: Int): Option[A] = {
    val a = sparse(k)
    if (a < members && dense(a) == k) {
      Some(map(k))
    } else {
      None
    }
  }

  def copyMap(ns: Int)(implicit manifest: Manifest[A]) = {
    val na = new Array[A](ns)
    var i = map.length - 1
    while (i >= 0) {
      na(i) = map(i)
      i -= 1
    }
    na

  }

  private def ensureCapacity(minCapacity: Int)(implicit manifest: Manifest[A]): SparseMap[A] = {
    val oldCapacity = sparse.length;

    //if (minCapacity > oldCapacity) {
      val newCapacity = math.max(minCapacity, (oldCapacity * 3) / 2 + 1);
      new SparseMap(
        Arrays.copyOf(dense, newCapacity),
        Arrays.copyOf(sparse, newCapacity),
        copyMap(newCapacity),
        members)
//    } else {
//      this
//    }

  }

  def +(t: (Int, A))(implicit manifest: Manifest[A]): SparseMap[A] = {
    val (k, v) = t
    val sm = ensureCapacity(k)
    val a = sm.sparse(k)
    val b = sm.members
    sm.map(k) = v
    if (a >= b || sm.dense(a) != k) {
      sm.sparse(k) = b
      sm.dense(b) = k
      new SparseMap(sm.dense, sm.sparse, sm.map, sm.members + 1)
    } else {
      sm
    }
  }

  def filter(f: Int => Boolean) = {
    var a = 0
    var m = members
    val dense = this.dense.clone
    val sparse = this.sparse.clone
    val map = this.map.clone
    while (a < m) {
      if (f(dense(a))) {
        a += 1
      } else {
        m -= 1
        val e = dense(m)
        dense(m) = dense(a)
        dense(a) = e
        sparse(e) = a
        sparse(a) = m
      }
    }
    new SparseMap(dense, sparse, map, m)
  }

  def mapValues[B](f: A => B)(implicit manifest: Manifest[B]): SparseMap[B] = {
    var i = 0
    val newMap = new Array[B](map.size)
    while (i < members) {
      val k = dense(i)
      newMap(k) = f(map(k))
      i += 1
    }
    new SparseMap[B](dense, sparse, newMap, members)
  }

  def iterator = new Iterator[(Int, A)] {
    var i = 0
    def hasNext = i < members
    def next = {
      val k = dense(i)
      i += 1
      (k, SparseMap.this.map(k))
    }
  }

  override def hashCode = MurmurHash3.unorderedHash(iterator)

  override def equals(o: Any) = o match {
    case t: SparseMap[A] => t.size == size && forall(e => t(e._1) == e._2)
    case _ => false

  }

  override def size = members

}