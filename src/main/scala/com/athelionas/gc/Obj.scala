package com.athelionas.gc

import scala.collection.immutable.Seq

case class Obj(name: String, children: Seq[Ref] = Seq.empty, private var marked: Boolean = false) {
  override def toString: String = s"$name: ${children.mkString(", ")}"

  def isMarked: Boolean = marked
  def nonMarked: Boolean = !marked
  def mark(): Unit = marked = true
  def unmark(): Unit = marked = false
}
