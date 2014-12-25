import java.util

import scala.collection.mutable

// do NOT support negative cycle
// take care of the vertex index
class ZKWCostFlow(src: Int, target: Int, totalVertexes: Int) {
  outer =>

  require(src < totalVertexes && target < totalVertexes)

  class Edge(
      val target: Int,
      val cost: Int,
      var capacity: Int,
      val nextEdgeIndex: Int,
      _residua: Int,
      val isRealEdge: Boolean) {
    def residua = edges(_residua)
  }

  class EdgeIterator(edgeIndex: Int) extends Iterator[Int] {
    var currentIndex = edgeIndex
    override def hasNext: Boolean = currentIndex > -1
    override def next(): Int = {
      val result = currentIndex
      currentIndex = edges(result).nextEdgeIndex
      result
    }
  }

  private val edges = mutable.ArrayBuffer.empty[Edge]
  private val head = Array.fill(totalVertexes)(-1)
  private val distance = new Array[Int](totalVertexes) // distance to the target
  private val visited = new Array[Boolean](totalVertexes)
  private val currentArc = new Array[Int](totalVertexes)
  private val flows = mutable.HashMap.empty[(Int, Int), Int]
  private var answer = 0
  private var finished = false

  private def getEdgesFrom(edgeIndex: Int): Iterator[Int] = new EdgeIterator(edgeIndex)
  implicit class Vertex(i: Int) {
    def distance = outer.distance(i)
  }

  private def augment(vertex: Int, flow: Int): Int = {

    def isShortestPath(edge: Edge) =
      vertex.distance == edge.target.distance + edge.cost

    def canAugment(edge: Edge) =
      !visited(edge.target) && edge.capacity > 0 && isShortestPath(edge)

    if (vertex == target) {
      answer += flow * src.distance
      flow
    } else {
      visited(vertex) = true
      var tmp = flow
      for (edgeIndex <- getEdgesFrom(currentArc(vertex)).takeWhile(_ => tmp > 0)) {
        val edge = edges(edgeIndex)
        if (canAugment(edge)) {
          val delta = augment(edge.target, math.min(tmp, edge.capacity))
          edge.capacity -= delta
          edge.residua.capacity += delta
          tmp -= delta
          currentArc(vertex) = edgeIndex
          if (edge.isRealEdge) {
            val path = vertex -> edge.target
            flows += path -> (flows.getOrElse(path, 0) + delta)
          } else {
            val path = edge.target -> vertex
            flows += path -> (flows(path) - delta)
          }
        }
      }
      flow - tmp
    }
  }

  private def modifyLabel(): Boolean = {

    def calculateDelta(edge: Edge, from: Int) =
      edge.target.distance + edge.cost - from.distance

    var delta = Int.MaxValue

    var vertex = 0
    while (vertex < totalVertexes) {
      if (visited(vertex)) {
        for (edgeIndex <- getEdgesFrom(head(vertex))) {
          val edge = edges(edgeIndex)
          if (edge.capacity > 0 && !visited(edge.target)) {
            delta = math.min(delta, calculateDelta(edge, vertex))
          }
        }
      }
      vertex += 1
    }

    if (delta == Int.MaxValue)
      false
    else {
      for (vertex <- 0 until totalVertexes) {
        if (visited(vertex)) {
          distance(vertex) += delta
          currentArc(vertex) = head(vertex)
          visited(vertex) = false
        }
      }
      true
    }
  }

  private def initDistance(): Unit = {
    val queue = new UniqueQueue[Int]
    queue += target
    distance(target) = 0
    while (queue.nonEmpty) {
      val vertex = queue.dequeue()
      for (edgeIndex <- getEdgesFrom(head(vertex))) {
        val edge = edges(edgeIndex)
        if (edge.residua.capacity > 0) {
          val tmp = vertex.distance - edge.cost // or + edge.residua.cost
          if (tmp < distance(edge.target)) {
            distance(edge.target) = tmp
            if (queue.isEmpty || queue.front.distance > tmp) {
              edge.target +=: queue
            } else {
              queue += edge.target
            }
          }
        }
      }
    }
  }

  def addEdge(from: Int, to: Int, cost: Int, capacity: Int): Unit = {
    val currentEdgesCount = edges.size
    edges += new Edge(to, cost, capacity, head(from), currentEdgesCount + 1, true)
    head(from) = currentEdgesCount
    edges += new Edge(from, -cost, 0, head(to), currentEdgesCount, false)
    head(to) = currentEdgesCount + 1
  }

  def zkw() = {
    require(!finished)

    initDistance()
    var vertex = 0
    while (vertex < totalVertexes) {
      currentArc(vertex) = head(vertex)
      vertex += 1
    }

    do {
      while (augment(src, Int.MaxValue) > 0)
        util.Arrays.fill(visited, false)
    } while (modifyLabel())
    finished = true
    answer -> flows.toMap
  }
}

object ZKWCostFlow extends App {
  val vertexes = Array("s", "t", "2", "3", "4")
  val s = new ZKWCostFlow(0, 1, vertexes.size)
  s.addEdge(0, 2, 4, 10)
  s.addEdge(0, 3, 1, 8)
  s.addEdge(3, 2, 2, 5)
  s.addEdge(3, 4, 3, 10)
  s.addEdge(2, 4, 6, 2)
  s.addEdge(2, 1, 1, 7)
  s.addEdge(4, 1, 2, 4)
  println(s.zkw())
}
