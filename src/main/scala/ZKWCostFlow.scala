import scala.collection.mutable

class ZKWCostFlow(src: String, target: String) {
  outer =>

  class Edge(
      val to: String,
      val cost: Int,
      var capacity: Int,
      val next: Int,
      _residua: Int,
      val isRealEdge: Boolean) {
    def residua = edges(_residua)
  }

  class EdgeIterator(edgeIndex: Int) extends Iterator[Edge] {
    var currentIndex = edgeIndex
    override def hasNext: Boolean = currentIndex > -1
    override def next(): Edge = {
      val result = edges(currentIndex)
      currentIndex = result.next
      result
    }
  }

  private val edges = mutable.ArrayBuffer.empty[Edge]
  private val head = mutable.HashMap.empty[String, Int]
  private val distance = mutable.HashMap.empty[String, Int]
  private val visited = mutable.HashSet.empty[String]
  private val flows = mutable.HashMap.empty[(String, String), Int]
  private var answer = 0
  private var finished = false
  private var minCost = Int.MaxValue

  private def getHead(v: String) = head.getOrElse(v, -1)
  private def getEdgesFrom(v: String) = new EdgeIterator(head(v))
  private def getEdgesFrom(edgeIndex: Int) = new EdgeIterator(edgeIndex)
  implicit class Vertex(s: String) {
    def distance = outer.distance(s)
  }

  private def augment(vertex: String, flow: Int): Int = {

    def isShortestPath(edge: Edge) =
      vertex.distance == edge.to.distance + edge.cost

    def canAugment(edge: Edge) =
      !visited(edge.to) && edge.capacity > 0 && isShortestPath(edge)

    if (vertex == target) {
      answer += flow * src.distance
      flow
    } else {
      visited += vertex
      var tmp = flow
      for (edge <- getEdgesFrom(vertex).takeWhile(_ => tmp > 0)) {
        if (canAugment(edge)) {
          val delta = augment(edge.to, tmp.min(edge.capacity))
          edge.capacity -= delta
          edge.residua.capacity += delta
          tmp -= delta
          if (edge.isRealEdge) {
            val path = vertex -> edge.to
            flows += path -> (flows.getOrElse(path, 0) + delta)
          } else {
            val path = edge.to -> vertex
            flows += path -> (flows(path) - delta)
          }
        }
      }
      flow - tmp
    }
  }

  private def modifyLabel(): Boolean = {

    def calculateDelta(edge: Edge, from: String) =
      edge.to.distance + edge.cost - from.distance

    var delta = Int.MaxValue
    for ((from, edgeIndex) <- head) {
      if (visited(from)) {
        for (edge <- getEdgesFrom(edgeIndex)) {
          if (edge.capacity > 0 && !visited(edge.to)) {
            delta = math.min(delta, calculateDelta(edge, from))
          }
        }
      }
    }

    if (delta == Int.MaxValue)
      false
    else {
      for (vertex <- head.keysIterator if visited(vertex)) {
        distance += vertex -> (vertex.distance + delta)
      }
      visited.clear()
      true
    }
  }

  private def initDistance(): Unit = {
    if (minCost < 0) {
      val queue = mutable.Queue.empty[String]
      queue += target
      distance += target -> 0
      while (queue.nonEmpty) {
        val vertex = queue.dequeue()
        for (edge <- getEdgesFrom(vertex)) {
          if (edge.residua.capacity > 0) {
            val tmp = vertex.distance - edge.cost // or + edge.residua.cost
            if (tmp < distance.getOrElse(edge.to, Int.MaxValue)) {
              distance += edge.to -> tmp
              if (queue.isEmpty || queue.front.distance > tmp) {
                edge.to +=: queue
              } else {
                queue += edge.to
              }
            }
          }
        }
      }
    } else {
      for (v <- head.keysIterator) distance += v -> 0
    }
  }

  def addEdge(from: String, to: String, cost: Int, capacity: Int): Unit = {
    if (cost < minCost) minCost = cost
    val currentEdgesCount = edges.size
    edges += new Edge(to, cost, capacity, getHead(from), currentEdgesCount + 1, true)
    head += from -> currentEdgesCount
    edges += new Edge(from, -cost, 0, getHead(to), currentEdgesCount, false)
    head += to -> (currentEdgesCount + 1)
  }

  def zkw() = {
    require(!finished && head.contains(src) && head.contains(target))
    initDistance()
    do {
      while (augment(src, Int.MaxValue) > 0) visited.clear()
    } while (modifyLabel())
    finished = true
    answer -> flows.toMap
  }

}

object ZKWCostFlow extends App {
  val s = new ZKWCostFlow("s", "t")
  s.addEdge("s", "2", 4, 10)
  s.addEdge("s", "3", 1, 8)
  s.addEdge("3", "2", 2, 5)
  s.addEdge("3", "4", 3, 10)
  s.addEdge("2", "4", 6, 2)
  s.addEdge("2", "t", 1, 7)
  s.addEdge("4", "t", 2, 4)

  println(s.zkw())
}
