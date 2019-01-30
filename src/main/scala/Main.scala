import scala.collection.immutable.Seq

object Main extends App {
  val garbageCollector = new GarbageCollector(
    Array(
      Obj("A", Seq(Ref(1), Ref(2))),
      Obj("B", Seq(Ref(1), Ref(2), Ref(3))),
      Obj("C", Seq(Ref(3), Ref(4))),
      Obj("D"),
      Obj("E"),
      Obj("F", Seq(Ref(4)))
    )
  )

  println(
    garbageCollector.gc(
      List(Ref(2), Ref(5))
    ).mkString("\n")
  )
}
