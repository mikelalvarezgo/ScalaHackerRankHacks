package InterviewPreparationKit

object DictionaryHashMaps {

  def twoStrings(s1: String, s2: String): String = {
    var found = false
    var i = 0
    val subsStr = (0 to s1.length).foldLeft(Set.empty[String]){(set, i) => s1.sliding(i).toSet ++ set }.toArray
    while (!found && i < subsStr.size){
      found = s2.contains(subsStr(i))
      i += 1
    }
    if(found) "YES" else "NO"
  }

  def twoStrings2(s1: String, s2: String): String = {
    var found = false
    var i = 0
    while (!found && i < s1.size){
        found = s1.sliding(0,i).toSet.intersect(s1.sliding(0,i).toSet).nonEmpty
      i += 1
    }
    if(found) "YES" else "NO"
  }

        def checkMagazine(magazine: Array[String], note: Array[String]) {
          // Write your code here

          val  matched = note.foldLeft((Array.empty[String], magazine)){
            case ((matchedNote , restOfMagazine), word) =>
              if (restOfMagazine.takeWhile(w => w != word).size < restOfMagazine.length)
                (matchedNote.appended(word), restOfMagazine diff Array(word))
              else (matchedNote, restOfMagazine)
            }._1
          //scalastyle:off
          if(matched.length == note.length) println("Yes") else println("No")
        }

    def sherlockAndAnagrams(s: String): Int = {
      // Write your code here
      val subStr = s.inits.flatMap(_.tails.toList.init).toList

      (2 to s.length).foldLeft(0){ (count,i) =>
        val subsStr = subStr.filter(_.size == i).map(_.sorted)
        val anagrams = (subsStr.size - subsStr.toSet.size) * 2
        count + anagrams
      }
    }

}
