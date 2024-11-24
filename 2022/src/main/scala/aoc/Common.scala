package aoc

import scala.collection.mutable

object Common {
  def timed[A](label: String, f: => A): Unit = {
    val start = System.currentTimeMillis()
    val res   = f
    val end   = System.currentTimeMillis()

    println(s"$label answer - $res It took ${end - start}ms")
  }

  extension [A](q: mutable.PriorityQueue[A])
    def enqueueAndKeepMaxSize(element: A, maxSize: Int): mutable.PriorityQueue[A] = {
      q.enqueue(element)
      if (q.size > maxSize) {
        q.dequeue()
      }

      q
    }
}
