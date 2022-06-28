package InterviewPreparationKit

import scala.collection.mutable.HashMap

object Graphs {
  def roadsAndLibraries(n: Int, c_lib: Int, c_road: Int, cities: Array[Array[Int]]): Long = {
    // Write your code here
    if(c_lib < c_road) n*c_lib else{
      val graph = HashMap.empty[Int, Set[Int]]
      cities.foreach(x => {
        graph += x.last -> graph.getOrElse(x.last, Set.empty[Int]).incl(x.head)
        graph += x.head -> graph.getOrElse(x.head, Set.empty[Int]).incl(x.last)
      })
      println(graph)
      var visited: Set[Int] = Set.empty
      var result = 0
      graph.map{i =>
        println(s"node ${i}")
        println(s"visited ... $visited")
        println(s"is visited ... ${visited.contains(i._1)}")
        if(visited.contains(i._1)) {
          println(result)
        }
        else {
          val isNeighbourVisited = i._2.exists(n => visited.contains(n))
          println(s"is neigh visited $isNeighbourVisited")
          if(! isNeighbourVisited) result += c_lib + (i._2.size *c_road)
          else result += c_road
          visited = visited.incl(i._1).union(i._2)

        }
      }
      result
    }

  }
}
