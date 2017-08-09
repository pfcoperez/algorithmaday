package org.pfcoperez.dailyalgorithm.applications

object RecoverWords extends App {

  import org.pfcoperez.dailyalgorithm.datastructures.graphs.directed.trees.Trie._

  /**
   * Restores a message whose "spaces" have been removed unsing a trie-dictionary
   *
   * O(n), n = Number of characters in the message
   *
   */
  def splitByDictionary(str: String, dictionary: TrieNode[Char, Unit]): Seq[String] = {

    val (_, acc, lastWordAcc, wasWord) = ((dictionary, List.empty[String], "", false) /: str) {
      case ((currentNode, wordsAcc, current, wasWord), c) =>
        val nextNode = currentNode.children.get(c)
        if (nextNode.isEmpty) {
          val newNode = dictionary.children.get(c).getOrElse(dictionary)
          val (newWord, validWord) = if (newNode != dictionary)
            s"$c" -> newNode.v.isDefined
          else
            "" -> false

          (newNode, if (wasWord) current :: wordsAcc else wordsAcc, newWord, validWord)
        } else
          (nextNode.get, wordsAcc, s"$current$c", nextNode.get.v.isDefined)

    }
    (if (wasWord) lastWordAcc :: acc else acc) reverse
  }

  val source = readLine
  val dictionary = (TrieNode[Char, Unit](None, Map.empty) /: readLine.split(' ').distinct) {
    (prevDict, word) => prevDict.insert(word)(())
  }

  val res = splitByDictionary(source, dictionary)

  res foreach {
    println
  }

}

