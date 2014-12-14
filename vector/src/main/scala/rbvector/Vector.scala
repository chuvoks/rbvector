/*
 * (c) 2003-2013, LAMP/EPFL 
 * Copyright (c) 2012-2014 Juha Heljoranta
 */
package rbvector

import rbvector.{ RedBlackRank => RB }
import scala.annotation.tailrec
import scala.collection.GenSeq
import scala.collection.GenTraversableOnce
import scala.collection.IndexedSeqLike
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.GenericCompanion
import scala.collection.generic.GenericTraversableTemplate
import scala.collection.generic.SeqFactory
import scala.collection.mutable.Builder
import javax.print.PrintService

/**
 * Companion object to the Vector class
 */
final object Vector extends SeqFactory[Vector] {

  override lazy val ReusableCBF: GenericCanBuildFrom[Nothing] =
    scala.collection.IndexedSeq.ReusableCBF.asInstanceOf[GenericCanBuildFrom[Nothing]]

  override def newBuilder[A]: Builder[A, Vector[A]] = new VectorBuilder[A]()

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Vector[A]] =
    ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

  private[this] val NIL = new Vector[Nothing]()

  override def empty[A]: Vector[A] = NIL

  def apply[A](a: A) = new Vector(RB.apply(a))

}

/**
 * IndexedSeq implementation with fast concatenation (++), patching and random element insertion & removal. 
 */
final class Vector[+A] private[rbvector] (private[rbvector] val tree: RB.Tree[A])
  extends IndexedSeq[A]
  with GenericTraversableTemplate[A, Vector]
  with IndexedSeqLike[A, Vector[A]]
  with Serializable { self =>

  def this() = this(null)

  override def companion: GenericCompanion[Vector] = Vector

  override def length = RB.count(tree)

  override def isEmpty = length == 0

  override def iterator: Iterator[A] = RB.iterator(tree)

  override def reverseIterator: Iterator[A] = RB.reverseIterator(tree)

  override def lengthCompare(len: Int): Int = {
    val l = length
    if (l > len) 1
    else if (l < len) -1
    else 0
  }

  override def apply(index: Int): A = RB.nth(tree, index).value

  override def foreach[U](f: A => U) = RB.foreach(tree, f)

  override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Vector[A], B, That]): That =
    if (bf eq IndexedSeq.ReusableCBF) new Vector(RB.map(tree, f)).asInstanceOf[That]
    else super.map(f)(bf)

  override def reverseMap[B, That](f: A => B)(implicit bf: CanBuildFrom[Vector[A], B, That]): That =
    if (bf eq IndexedSeq.ReusableCBF) new Vector(RB.reverseMap(tree, f)).asInstanceOf[That]
    else super.reverseMap(f)(bf)

  override def reverse = new Vector(RB.reverse(tree))

  override def head =
    if (isEmpty) throw new UnsupportedOperationException("empty.head")
    else RB.first(tree).value

  override def last =
    if (isEmpty) throw new UnsupportedOperationException("empty.last")
    else RB.last(tree).value

  override def tail =
    if (isEmpty) throw new UnsupportedOperationException("empty.tail")
    else new Vector(RB.deleteNth(tree, 0))

  override def init =
    if (isEmpty) throw new UnsupportedOperationException("empty.init")
    else new Vector(RB.deleteNth(tree, size - 1))

  override def take(n: Int): Vector[A] =
    if (n <= 0) Vector.empty
    else if (n < length) new Vector(RB.take(tree, n))
    else this

  override def drop(n: Int): Vector[A] =
    if (n <= 0) this
    else if (n < length) new Vector(RB.drop(tree, n))
    else Vector.empty

  override def takeRight(n: Int): Vector[A] = if (n > 0) drop(length - n) else Vector.empty

  override def dropRight(n: Int): Vector[A] = if (n > 0) take(length - n) else this

  override def splitAt(n: Int) = (take(n), drop(n))

  override def slice(from: Int, until: Int) = {
    if (until <= from) Vector.empty
    else if (from <= 0) take(until)
    else if (until >= size) drop(from)
    else new Vector(RB.slice(tree, from, until))
  }

  private[this] def countWhile(p: A => Boolean): Int = {
    var n = 0
    RB.forwardWhile(tree, { a: A => if (p(a)) { n += 1; true } else false })
    n
  }

  override def takeWhile(p: A => Boolean) = take(countWhile(p))
  override def dropWhile(p: A => Boolean) = drop(countWhile(p))
  override def span(p: A => Boolean) = splitAt(countWhile(p))

  override def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Vector[A], B, That]): That =
    if ((bf eq IndexedSeq.ReusableCBF) && that.isInstanceOf[Vector[_]]) new Vector(RB.concat(tree, that.asInstanceOf[Vector[B]].tree)).asInstanceOf[That]
    else super.++(that)(bf)

  override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Vector[A], B, That]): That =
    if (bf eq IndexedSeq.ReusableCBF) new Vector(RB.insertNth(tree, 0, elem)).asInstanceOf[That]
    else super.+:(elem)(bf)

  override def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Vector[A], B, That]): That =
    if (bf eq IndexedSeq.ReusableCBF) new Vector(RB.insertNth(tree, size, elem)).asInstanceOf[That]
    else super.:+(elem)(bf)

  override def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[Vector[A], B, That]): That =
    if (bf eq IndexedSeq.ReusableCBF) {
      if (index < 0 || index >= length) throw new IndexOutOfBoundsException(index.toString)
      new Vector(RB.updateNth(tree, index, elem, overwrite = true)).asInstanceOf[That]
    } else super.updated(index, elem)(bf)

  /** Same as patch(index, Repr(elem), 0) but faster */
  def inserted[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[Vector[A], B, That]): That =
    if (bf eq IndexedSeq.ReusableCBF) {
      val idx = if (index < 0) 0 else if (index > length) length else index
      new Vector(RB.insertNth(tree, idx, elem)).asInstanceOf[That]
    } else super.patch(index, Vector.apply(elem), 0)(bf)

  /** Same as patch(index, Repr.empty, 1) but faster */
  def removed[B >: A, That](index: Int)(implicit bf: CanBuildFrom[Vector[A], B, That]): That =
    if (bf eq IndexedSeq.ReusableCBF) {
      if (index >= length) this.asInstanceOf[That]
      else new Vector(RB.deleteNth(tree, if (index < 0) 0 else index)).asInstanceOf[That]
    } else super.patch(index, Vector.empty, 1)(bf)

  override def patch[B >: A, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[Vector[A], B, That]): That =
    if ((bf eq IndexedSeq.ReusableCBF)) {
      val fromSafe = math.min(math.max(0, from), length)
      val replacedSafe = math.min(math.max(0, replaced), length)
      val insert = patch.nonEmpty
      val delete = replacedSafe > 0
      if (insert || delete) {
        val insertOne = patch.seq.lengthCompare(1) == 0
        val deleteOne = replacedSafe == 1 && fromSafe < length
        if (insertOne && deleteOne) updated(fromSafe, patch.head)
        else if (insertOne && !delete) inserted(fromSafe, patch.head)
        else if (!insert && deleteOne) removed(fromSafe)
        else {
          val prefix = take(fromSafe)
          val rest = drop(fromSafe + replacedSafe)
          ((prefix ++ patch).asInstanceOf[Vector[B]] ++ rest).asInstanceOf[That]
        }
      } else this.asInstanceOf[That]
    } else super.patch(from, patch, replaced)(bf)

}

final class VectorBuilder[A]() extends Builder[A, Vector[A]] {

  private[this] var acc = RB.empty[A]

  private[this] val ib = new IterableBuilder[A]

  override def sizeHint(size: Int) = ib.sizeHint(size)

  override def +=(elem: A): this.type = { ib += elem; this }

  private[this] def commitIb() {
    if (ib.nonEmpty) {
      val iterable = ib.result
      val iterator = iterable.iterator
      acc = RB.concat(acc, RB.fill(RB.allocate(iterable.size), iterator.next()))
      ib.clear()
    }
  }

  override def ++=(xs: TraversableOnce[A]): this.type = {
    xs.seq match {
      case b: Vector[A] => {
        commitIb()
        acc = RB.concat(acc, b.tree)
        this
      }
      case v: IndexedSeqLike[A, _] => { // we need only fast size() method (and fast iterator)
        commitIb()
        val i = v.iterator
        acc = RB.concat(acc, RB.fill(RB.allocate(v.size), i.next()))
        this
      }
      case seq => { seq foreach ib.+=; this }
    }
  }

  override def result: Vector[A] = { commitIb(); new Vector(acc) }

  override def clear(): Unit = {
    ib.clear()
    acc = RB.empty[A]
  }

}
