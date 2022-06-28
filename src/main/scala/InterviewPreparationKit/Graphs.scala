package InterviewPreparationKit

import scala.collection.mutable
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
        if(visited.contains(i._1)) {
          println(result)
        }
        else {
          val isNeighbourVisited = i._2.exists(n => visited.contains(n))
          if(! isNeighbourVisited) result += c_lib + (i._2.size *c_road)
          else result += c_road
          visited = visited.incl(i._1).union(i._2)

        }
      }
      result
    }

  }

  def bfs(n: Int, m: Int, edges: Array[Array[Int]], s: Int): Array[Int] = {
    // Write your code here
    val graph = edges.foldLeft(HashMap.empty[Int, Set[Int]]){(graph,edge) =>
      graph
        .addOne(edge.head -> graph.getOrElse(edge.head, Set.empty[Int]).incl(edge.last))
    }
    println(graph)
    final case class State(visited: Set[Int], pending: mutable.Queue[Int], nodesOfLevel: Int, nodesOfNextLevel: Int, depth: Int){
      def step(int: Int): State = {
        copy(
          visited =  visited.incl(int),
          pending =  pending.enqueueAll(graph.getOrElse(int, Set.empty)),
          nodesOfLevel = nodesOfLevel -1,
          nodesOfNextLevel = nodesOfNextLevel + graph.getOrElse(int, Set.empty).size
        )
      }
      def checkDepth: State =
        if(nodesOfLevel == 0) copy(nodesOfLevel = nodesOfNextLevel, nodesOfNextLevel = 0, depth = depth + 1) else this

      def hasVisited(node: Int) = visited.contains(node)
    }
    var state = State(Set(s), mutable.Queue.from(graph.getOrElse(s, Set.empty)), graph.getOrElse(s, Set.empty).size, 0, 1)
    var result = Map.empty[Int, Int]
    while (state.pending.nonEmpty){
      val currentNode = state.pending.dequeue()
      if(! state.hasVisited(currentNode)){
        result  += currentNode -> 6 * state.depth
        state = state.step(currentNode).checkDepth
      }
    }
    ((1 to n).toSet -- state.visited).map(i => result += i-> -1)
    result.toSeq.sortWith(_._1 < _._1).map(_._2).toArray
  }
}
