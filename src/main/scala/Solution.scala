/**
 * Created by cloud0fan on 12/24/14.
 */
object Solution extends App {

  def transportPlan(rich: Seq[(String, Int)], poor: Seq[(String, Int)], costs: Array[Array[Int]]) = {
    val vertexes = (rich ++ poor).map(_._1)
    val srcIndex = vertexes.size
    val targetIndex = vertexes.size + 1
    val s = new ZKWCostFlow(srcIndex, targetIndex, vertexes.size + 2)
    for (i <- rich.indices) s.addEdge(srcIndex, i, 0, rich(i)._2)
    for (i <- poor.indices) s.addEdge(rich.size + i, targetIndex, 0, poor(i)._2)
    for (i <- 0 until rich.size; j <- 0 until poor.size) {
      s.addEdge(i, rich.size + j, costs(i)(j), Int.MaxValue)
    }
    val (cost, plan) = s.zkw()
    cost -> plan.filter { case ((from, to), flow) =>
      from != srcIndex && to != targetIndex
    }.map { case ((from, to), flow) =>
      ((vertexes(from), vertexes(to)), flow)
    }
  }

  println(transportPlan(Seq("a" -> 3, "b" -> 4), Seq("c" -> 5, "d" -> 2), Array(Array(1, 2), Array(2, 3))))
}
