package concrete.util

import org.eclipse.collections.impl.map.mutable.primitive.IntIntHashMap

import scala.collection.{SpecificIterableFactory, mutable}
import scala.jdk.CollectionConverters._

object IntIntMap extends SpecificIterableFactory[(Int, Int), IntIntMap] {
  override def newBuilder: mutable.Builder[(Int, Int), IntIntMap] =
    new mutable.GrowableBuilder[(Int, Int), IntIntMap](empty)

  override def fromSpecific(it: IterableOnce[(Int, Int)]): IntIntMap =
    mutable.Growable.from(empty, it)

  override def empty: IntIntMap = new IntIntMap()
}

class IntIntMap(private val intIntMap: IntIntHashMap = new IntIntHashMap())
  extends mutable.AbstractMap[Int, Int] with mutable.MapOps[Int, Int, mutable.Map, IntIntMap] {

  override def empty = new IntIntMap()

  override protected def fromSpecific(coll: IterableOnce[(Int, Int)]): IntIntMap =
    IntIntMap.fromSpecific(coll)

  override protected def newSpecificBuilder: mutable.Builder[(Int, Int), IntIntMap] =
    IntIntMap.newBuilder

  def this(initialCapacity: Int) = this(new IntIntHashMap(initialCapacity))

  override def addOne(kv: (Int, Int)): IntIntMap.this.type = {
    justPut(kv._1, kv._2)
    this
  }

  def justPut(key: Int, value: Int): Unit = intIntMap.put(key, value)

  override def subtractOne(key: Int): IntIntMap.this.type = {
    intIntMap.removeKey(key)
    this
  }

  override def apply(key: Int): Int = {
    intIntMap.getOrThrow(key)
  }

  override def get(key: Int): Option[Int] = {
    if (intIntMap.containsKey(key)) {
      Some(intIntMap.get(key))
    } else {
      None
    }
  }

  def getOrElse(key: Int, default: Int): Int = intIntMap.getIfAbsent(key, default)

  override def iterator: Iterator[(Int, Int)] = intIntMap.keyValuesView().iterator().asScala
    .map(pair => (pair.getOne, pair.getTwo))

}
