package com.athelionas.gc

import scala.collection.mutable

class GarbageCollector(private val heap: mutable.ArrayBuffer[Option[Obj]], val stack: mutable.ListBuffer[Ref]) {
  def gc(): this.type = {
    // Time complexity (worst case): O(n*(|V|+|E|)) where n = # of refs on stack, V = # of objects, E = # of refs in objects
    // Space complexity (worst case): O(|V|) where V = # of objects
    def mark(): Unit = {
      val unvisited = mutable.Queue(stack.toList:_*)

      while (unvisited.nonEmpty) {
        val current = heap(unvisited.dequeue.underlying).get

        if (current.nonMarked) {
          current.children.foreach(unvisited.enqueue(_))
          current.mark()
        }
      }
    }

    // Time complexity (worst case): O(n) where n = # of objects
    // Space complexity (worst case): O(1)
    def sweep(): Unit = for (i <- heap.indices) {
      heap(i) match {
        case Some(obj) if obj.isMarked => obj.unmark()
        case Some(obj) if obj.nonMarked => heap.update(i, None)
        case None =>
      }
    }

    mark()
    sweep()

    this
  }

  def compact(): this.type = {
    import scala.util.control.Breaks._

    for (i <- heap.indices) {
      heap(i) match {
        case None =>
          breakable {
            for (j <- i until heap.size) {
              heap(j) match {
                case Some(_) =>
                  heap.update(i, heap(j))
                  heap.update(j, None)
                  heap.filter(obj => obj.nonEmpty && obj.get.children.exists(_.underlying == j)).foreach(
                    obj => heap.update(
                      heap.indexOf(obj),
                      Some(
                        obj.get.copy(
                          children = obj.get.children.filterNot(_.underlying == j) :+ Ref(i)
                        )
                      )
                    )
                  )

                  val stackIndex = stack.indexOf(Ref(j))
                  if (stackIndex >= 0)
                    stack.update(stackIndex, Ref(i))

                  break
                case None => // NOOP
              }
            }
          }
        case Some(_) => // NOOP
      }
    }

    val startOfEmptySpace = heap.indexOf(None)
    heap.remove(startOfEmptySpace, heap.size - startOfEmptySpace)

    this
  }
}
