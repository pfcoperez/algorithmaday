object RecoverWords extends App {

  case class TrieNode[KE, V](v: Option[V], children: Map[KE, TrieNode[KE, V]]) {

    def insert(k: Seq[KE])(newVal: V): TrieNode[KE, V] = k match {
      case Seq() => copy(v = Some(newVal))
      case _ =>
        val keyElement = k.head
        val newChild = keyElement -> children.getOrElse(keyElement, TrieNode[KE, V](None, Map.empty)).insert(k.tail)(newVal)
        copy(children = children + newChild)
    }

    def find(k: Seq[KE]): Option[V] = k.headOption flatMap { keyElement =>
      for(child <- children.get(keyElement); res <- child.find(k.tail)) yield res
    } orElse v

    def contains(k: Seq[KE]): Boolean = find(k).isDefined

  }

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
        if(nextNode.isEmpty) {
          val newNode = dictionary.children.get(c).getOrElse(dictionary)
          val (newWord, validWord) = if(newNode != dictionary)
            s"$c" -> newNode.v.isDefined
          else
            "" -> false

          (newNode, if(wasWord) current::wordsAcc else wordsAcc, newWord, validWord)
        }
        else
          (nextNode.get, wordsAcc, s"$current$c", nextNode.get.v.isDefined)

    }
    (if(wasWord) lastWordAcc::acc else acc) reverse
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

