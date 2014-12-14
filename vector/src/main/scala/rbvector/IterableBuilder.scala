/*
 * Copyright (c) 2012-2014 Juha Heljoranta
 */
package rbvector

import scala.collection.mutable.Builder

final class IterableBuilder[A]() extends Builder[A, Iterable[A]] {
  builder =>

  private[this] var arr: Array[A] = mkArr

  private[this] var len = 0;

  private[this] var hintSize = -1;

  def nonEmpty = len > 0

  override def sizeHint(size: Int) = hintSize = size // used in ensureCapacity()

  override def +=(a: A): this.type = {
    ensureCapacity()
    arr(len) = a
    len += 1
    this
  }
  override def ++=(xs: TraversableOnce[A]): this.type =
    super.++=(xs)

  override def clear() { arr = mkArr; len = 0; hintSize = -1 }

  override def result: Iterable[A] = new Iterable[A] {
    iterable =>

    // copy from builder
    private[this] val a: Array[A] = builder.arr
    override val size: Int = builder.len

    override def iterator: Iterator[A] = new Iterator[A] {

      private[this] var i = 0

      @inline override final def hasNext = i < iterable.size

      override final def next =
        if (hasNext) { val e = a(i); i += 1; e }
        else Iterator.empty.next

      override def foreach[U](f: A => U) { while (hasNext) { f(a(i)); i += 1 } }

//      override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit = {
//        require(start >= 0 && (start < xs.length || xs.length == 0), s"start $start out of range ${xs.length}")
//        val n = math.min(iterable.size - i, math.min(len, xs.length - start))
//        if (n > 0) {
//          System.arraycopy(a, i, xs, start, n);
//          i += n
//        }
//      }

    }

//    override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) {
//      val n = math.min(size, math.min(len, xs.length))
//      if (n > 0) System.arraycopy(a, 0, xs, start, n);
//    }
  }

  private[this] def ensureCapacity() {
    val minSize = len + 1
    if (minSize > arr.length) {
      val len = arr.length;
      var newLen = len + (len >> 1);
      if (newLen - minSize < 0) newLen = minSize;
      arr = resize(math.max(newLen, hintSize))
    }
  }

  private[this] def mkArr: Array[A] =
    new Array[AnyRef](10).asInstanceOf[Array[A]]

  private[this] def resize(n: Int): Array[A] =
    java.util.Arrays.copyOf(arr.asInstanceOf[Array[AnyRef]], n).asInstanceOf[Array[A]]

}
