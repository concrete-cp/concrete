package cspfj.constraint.extension

object TrieMap {
//  def empty[A] = new TrieMap(Map[Array[A], TrieMap[A]](), null)
//}
//
//class TrieMap[A](val trie: Map[A, TrieMap[A]], val leaf: A) extends Map[Array[A], A] {
//
//  def +(tuple: Array[A], value: A, i: Int): TrieMap[A] = {
//    if (i >= tuple.length) new TrieMap(Map.empty, value)
//    else {
//      val v = tuple(i)
//      //ensureCapacity(v + 1)
//
//      val subTrie = trie.getOrElse(v, TrieMap.empty)
//      
//      new TrieMap(
//        trie + (v -> (subTrie + (tuple, value, i + 1))), value)
//
//    }
//  }
}