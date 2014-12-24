/**
 * Created by cloud0fan on 12/24/14.
 */
object Solution extends App {

  def transportPlan(rich: Seq[(String, Int)], poor: Seq[(String, Int)], costs: Array[Array[Int]]) = {
    val s = new ZKWCostFlow("s", "t")
    for ((name, extra) <- rich) s.addEdge("s", name, 0, extra)
    for ((name, need) <- poor) s.addEdge(name, "t", 0, need)
    for (i <- 0 until rich.size; j <- 0 until poor.size) {
      s.addEdge(rich(i)._1, poor(j)._1, costs(i)(j), Int.MaxValue)
    }
    val (cost, plan) = s.zkw()
    cost -> plan.filter {
      case ((from, to), flow) => from != "s" && to != "t"
    }
  }

  println(transportPlan(Seq("a" -> 3, "b" -> 4), Seq("c" -> 5, "d" -> 2), Array(Array(1, 2), Array(2, 3))))
}
