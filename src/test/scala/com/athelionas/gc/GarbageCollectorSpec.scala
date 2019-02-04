package com.athelionas.gc

import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

import scala.collection.immutable.Seq
import scala.collection.mutable

class GarbageCollectorSpec extends FlatSpec with Matchers with PrivateMethodTester {
  private implicit class PimpedGC(underlying: GarbageCollector) {
    def view: Array[Option[Obj]] = {
      underlying invokePrivate PrivateMethod[mutable.ArrayBuffer[Option[Obj]]]('heap)()
    }.toArray
  }

  "GC" must "not leave any marked object dangling in memory" in {
    val garbageCollector = new GarbageCollector(
      mutable.ArrayBuffer(
        Some(Obj("A", Seq(Ref(1), Ref(2)))),
        Some(Obj("B", Seq(Ref(1), Ref(2), Ref(3)))),
        Some(Obj("C", Seq(Ref(3), Ref(4)))),
        Some(Obj("D")),
        Some(Obj("E")),
        Some(Obj("F", Seq(Ref(4))))
      ),
      mutable.ListBuffer(Ref(2), Ref(5))
    )

    garbageCollector.gc()

    assert(
      garbageCollector.view.collect {
        case Some(obj) if obj.isMarked => obj
      }.isEmpty
    )
  }

  it must "be able to handle cyclic references" in {
    val garbageCollector = new GarbageCollector(
      mutable.ArrayBuffer(
        Some(Obj("A", Seq(Ref(1)))),
        Some(Obj("B", Seq(Ref(2)))),
        Some(Obj("C", Seq(Ref(3)))),
        Some(Obj("D", Seq(Ref(4)))),
        Some(Obj("E", Seq(Ref(5)))),
        Some(Obj("F", Seq(Ref(0)))),
      ),
      mutable.ListBuffer.empty
    )

    garbageCollector.gc()

    assert(
      garbageCollector.view.forall(_.isEmpty)
    )
  }

  "Compaction" must "not leave gaps between allocated objects in memory, nor leading/trailing empty spaces" in {
    val garbageCollector = new GarbageCollector(
      mutable.ArrayBuffer(
        None,
        Some(Obj("A")),
        None,
        Some(Obj("B")),
        Some(Obj("C")),
        None,
        None,
        Some(Obj("D")),
        None,
        Some(Obj("E")),
        Some(Obj("F")),
        None,
        None
      ),
      mutable.ListBuffer.empty
    )

    garbageCollector.compact()

    assert(
      garbageCollector.view.forall(_.nonEmpty)
    )
  }

  it must "update corresponding references of moved objects on heap" in {
    val garbageCollector = new GarbageCollector(
      mutable.ArrayBuffer(
        None,
        Some(Obj("A")),
        None,
        Some(Obj("B", Seq(Ref(1)))),
        Some(Obj("C", Seq(Ref(1), Ref(7)))),
        None,
        None,
        Some(Obj("D", Seq(Ref(9)))),
        None,
        Some(Obj("E")),
        Some(Obj("F", Seq(Ref(3), Ref(7)))),
        None,
        None
      ),
      mutable.ListBuffer.empty
    )

    def referencedObjectNames= garbageCollector.view.collect {
      case Some(obj) => obj.children
    }.flatten.distinct.flatMap(ref => garbageCollector.view(ref.underlying).map(_.name))

    val before = referencedObjectNames

    garbageCollector.compact()

    val after = referencedObjectNames

    assert(before.deep == after.deep)
  }

  it must "update corresponding references of moved objects on stack" in {
    val garbageCollector = new GarbageCollector(
      mutable.ArrayBuffer(
        None,
        Some(Obj("A")),
        None,
        Some(Obj("B", Seq(Ref(1)))),
        Some(Obj("C", Seq(Ref(1), Ref(7)))),
        None,
        None,
        Some(Obj("D", Seq(Ref(9)))),
        None,
        Some(Obj("E")),
        Some(Obj("F", Seq(Ref(3), Ref(7)))),
        None,
        None
      ),
      mutable.ListBuffer(
        Ref(1),
        Ref(3),
        Ref(4),
        Ref(7),
        Ref(9),
        Ref(10)
      )
    )

    def referencedObjectNames = garbageCollector.stack.map(
      ref => garbageCollector.view(ref.underlying).get.name
    ).toList

    val before = referencedObjectNames

    garbageCollector.compact()

    val after = referencedObjectNames

    assert(before == after)
  }
}
