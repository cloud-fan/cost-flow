import scala.collection.mutable

/**
 * Created by cloud0fan on 12/24/14.
 */
class UniqueQueue[A] {

  private val queue = mutable.Queue.empty[A]
  private val isInQueue = mutable.HashSet.empty[A]

  private def add(element: A, back: Boolean): Unit = {
    if (!isInQueue(element)) {
      if (back) queue += element else element +=: queue
      isInQueue += element
    }
  }

  def +=(element: A): Unit = add(element, true)

  def +=:(element: A): Unit = add(element, false)

  def dequeue() = {
    val result = queue.dequeue()
    isInQueue -= result
    result
  }

  def front = queue.front
  def nonEmpty = queue.nonEmpty
  def isEmpty = queue.isEmpty
}

