package org.pfcoperez.dailyalgorithm.datastructures.graphs.directed.trees

object Trie {

  case class TrieNode[KE, V](v: Option[V], children: Map[KE, TrieNode[KE, V]]) {

    /**
     * Returns a new Trie built upon 'this' which contains (k, newVal)
     *
     * O(n), n = length of 'k'
     */
    def insert(k: Seq[KE])(newVal: V): TrieNode[KE, V] = k match {
      case Seq() => copy(v = Some(newVal))
      case _ =>
        val keyElement = k.head
        val newChild = keyElement -> children.getOrElse(keyElement, TrieNode[KE, V](None, Map.empty)).insert(k.tail)(newVal)
        copy(children = children + newChild)
    }

    /**
     * Look for the key 'k' in the trie.
     * @param k
     * @return The Some(value) associated to the key in the Trie or None if not found
     *
     * O(n), n = length of 'k'
     */
    def find(k: Seq[KE]): Option[V] = k.headOption flatMap { keyElement =>
      for (child <- children.get(keyElement); res <- child.find(k.tail)) yield res
    } orElse v

    def contains(k: Seq[KE]): Boolean = find(k).isDefined

  }

}
