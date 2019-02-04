package com.athelionas.gc

case class Ref(underlying: Int) extends AnyVal {
  override def toString: String = underlying.toString
}
