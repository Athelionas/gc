import scala.collection.immutable.Seq

case class Ref(underlying: Int) extends AnyVal {
  override def toString: String = underlying.toString
}

case class Obj(name: String, children: Seq[Ref] = Seq.empty) {
  override def toString: String = s"$name: ${children.mkString(", ")}"
}

class GarbageCollector(private var heap: Array[Obj]) {
  def gc(stack: List[Ref]): Array[Obj] = {
    def traverse(refs: Seq[Ref], reachable: Seq[Ref] = Seq.empty): Array[Obj] = {
      val unvisited = refs.distinct.filterNot(reachable.contains)

      unvisited.map(ref => heap(ref.underlying)).toArray ++ {
        if (unvisited.nonEmpty) {
          traverse(unvisited.flatMap(ref => heap(ref.underlying).children), unvisited ++ reachable)
        } else Array.empty[Obj]
      }
    }

    traverse(stack).sortBy(_.name)
  }
}
