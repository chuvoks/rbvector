/*
 * (c) 2003-2013, LAMP/EPFL 
 * Copyright (c) 2012-2014 Juha Heljoranta
 */
package rbvector

import annotation.tailrec
import annotation.meta.getter
import java.util.ArrayDeque

final object RedBlackRank {

  def isEmpty(tree: Tree[_]): Boolean = tree eq null

  def empty[B]: Tree[B] = null

  def apply[B](v: B): Tree[B] = BlackTree(v, null, null)

  def count(tree: Tree[_]) = if (tree eq null) 0 else tree.count

  def insertNth[B](tree: Tree[B], index: Int, v: B): Tree[B] = blacken(insNth(tree, index + 1, v))
  def deleteNth[B](tree: Tree[B], index: Int): Tree[B] = blacken(del(tree, index + 1))
  def updateNth[B, B1 >: B](tree: Tree[B], n: Int, v: B1, overwrite: Boolean): Tree[B1] = blacken(setNth(tree, n + 1, v))

  def concat[B](left: Tree[B], right: Tree[B]): Tree[B] = blacken(doConcat(left, right))

  def take[B](tree: Tree[B], n: Int): Tree[B] = blacken(doTake(tree, n))
  def slice[B](tree: Tree[B], from: Int, until: Int): Tree[B] = blacken(doSlice(tree, from, until))
  def drop[B](tree: Tree[B], n: Int): Tree[B] = blacken(doDrop(tree, n))

  def first[B](tree: Tree[B]): Tree[B] = {
    if (tree eq null) throw new NoSuchElementException("empty map")
    var result = tree
    while (result.left ne null) result = result.left
    result
  }
  def last[B](tree: Tree[B]): Tree[B] = {
    if (tree eq null) throw new NoSuchElementException("empty map")
    var result = tree
    while (result.right ne null) result = result.right
    result
  }

  def foreach[B, U](tree: Tree[B], f: B => U): Unit = if (tree ne null) _foreach(tree, f)
  private[this] def _foreach[B, U](tree: Tree[B], f: B => U) {
    if (tree.left ne null) _foreach(tree.left, f)
    f(tree.value)
    if (tree.right ne null) _foreach(tree.right, f)
  }

  def map[B, U](tree: Tree[B], f: B => U): Tree[U] = if (tree eq null) null else _map(tree, f)
  private[this] def _map[B, U](tree: Tree[B], f: B => U): Tree[U] = {
    val l = if (tree.left ne null) _map(tree.left, f) else null
    val v = f(tree.value)
    val r = if (tree.right ne null) _map(tree.right, f) else null
    mkTree(isBlackTree(tree), v, l, r)
  }

  private[rbvector] def fill[U](tree: Tree[_], f: => U): Tree[U] = if (tree eq null) null else _fill(tree, f)
  private[this] def _fill[U](tree: Tree[_], f: => U): Tree[U] = {
    val l = if (tree.left ne null) _fill(tree.left, f) else null
    val v = f
    val r = if (tree.right ne null) _fill(tree.right, f) else null
    mkTree(isBlackTree(tree), v, l, r)
  }

  def reverseMap[B, U](tree: Tree[B], f: B => U): Tree[U] = if (tree eq null) null else _reverseMap(tree, f)
  private[this] def _reverseMap[B, U](tree: Tree[B], f: B => U): Tree[U] = {
    val r = if (tree.right ne null) _reverseMap(tree.right, f) else null
    val v = f(tree.value)
    val l = if (tree.left ne null) _reverseMap(tree.left, f) else null
    mkTree(isBlackTree(tree), v, r, l)
  }

  def reverse[B](tree: Tree[B]): Tree[B] = if (tree eq null) null else _reverse(tree)
  private[this] def _reverse[B](tree: Tree[B]): Tree[B] = {
    val l = if (tree.left ne null) _reverse(tree.left) else null
    val r = if (tree.right ne null) _reverse(tree.right) else null
    mkTree(isBlackTree(tree), tree.value, r, l)
  }

  private[rbvector] def forwardWhile[B](tree: Tree[B], p: B => Boolean): Boolean = if (tree eq null) true else _forwardWhile(tree, p)
  private[this] def _forwardWhile[B](tree: Tree[B], p: B => Boolean): Boolean = {
    ((tree.left eq null) || _forwardWhile(tree.left, p)) &&
      p(tree.value) &&
      ((tree.right eq null) || _forwardWhile(tree.right, p))
  }

  def iterator[B](tree: Tree[B], focus: Option[Int] = None): Iterator[B] = new TreeIteratorFwd(tree, focus)
  def reverseIterator[B](tree: Tree[B], focus: Option[Int] = None): Iterator[B] = new TreeIteratorBwd(tree, focus)

  @tailrec
  def nth[B](tree: Tree[B], n: Int): Tree[B] = {
    val count = this.count(tree.left)
    if (n < count) nth(tree.left, n)
    else if (n > count) nth(tree.right, n - count - 1)
    else tree
  }

  def isBlack(tree: Tree[_]) = (tree eq null) || isBlackTree(tree)

  private[this] def isRedTree(tree: Tree[_]) = tree.isInstanceOf[RedTree[_]]
  private[this] def isBlackTree(tree: Tree[_]) = tree.isInstanceOf[BlackTree[_]]

  private[this] def blacken[B](t: Tree[B]): Tree[B] = if (t eq null) null else t.black

  private[this] def mkTree[B](isBlack: Boolean, v: B, l: Tree[B], r: Tree[B]) =
    if (isBlack) BlackTree(v, l, r) else RedTree(v, l, r)

  private[this] def balanceLeft[B, B1 >: B](isBlack: Boolean, zv: B, l: Tree[B1], d: Tree[B1]): Tree[B1] = {
    if (isRedTree(l) && isRedTree(l.left))
      RedTree(l.value, BlackTree(l.left.value, l.left.left, l.left.right), BlackTree(zv, l.right, d))
    else if (isRedTree(l) && isRedTree(l.right))
      RedTree(l.right.value, BlackTree(l.value, l.left, l.right.left), BlackTree(zv, l.right.right, d))
    else
      mkTree(isBlack, zv, l, d)
  }
  private[this] def balanceRight[B, B1 >: B](isBlack: Boolean, xv: B, a: Tree[B1], r: Tree[B1]): Tree[B1] = {
    if (isRedTree(r) && isRedTree(r.left))
      RedTree(r.left.value, BlackTree(xv, a, r.left.left), BlackTree(r.value, r.left.right, r.right))
    else if (isRedTree(r) && isRedTree(r.right))
      RedTree(r.value, BlackTree(xv, a, r.left), BlackTree(r.right.value, r.right.left, r.right.right))
    else
      mkTree(isBlack, xv, a, r)
  }

  private[this] def setNth[B, B1 >: B](tree: Tree[B], idx: Int, v: B1): Tree[B1] = if (tree eq null) {
    null
  } else {
    val rank = count(tree.left) + 1
    if (idx < rank) mkTree(isBlackTree(tree), tree.value, setNth(tree.left, idx, v), tree.right)
    else if (idx > rank) mkTree(isBlackTree(tree), tree.value, tree.left, setNth(tree.right, idx - rank, v))
    else mkTree(isBlackTree(tree), v, tree.left, tree.right)
  }
  private[this] def updNth[B, B1 >: B](tree: Tree[B], idx: Int, v: B1, overwrite: Boolean): Tree[B1] = if (tree eq null) {
    RedTree(v, null, null)
  } else {
    val rank = count(tree.left) + 1
    if (idx < rank) balanceLeft(isBlackTree(tree), tree.value, updNth(tree.left, idx, v, overwrite), tree.right)
    else if (idx > rank) balanceRight(isBlackTree(tree), tree.value, tree.left, updNth(tree.right, idx - rank, v, overwrite))
    else if (overwrite) mkTree(isBlackTree(tree), v, tree.left, tree.right)
    else tree
  }
  private[this] def insNth[B](tree: Tree[B], idx: Int, v: B): Tree[B] = if (tree eq null) {
    RedTree(v, null, null)
  } else { // shift elements at idx to right
    val rank = count(tree.left) + 1
    if (idx <= rank) balanceLeft(isBlackTree(tree), tree.value, insNth(tree.left, idx, v), tree.right)
    else balanceRight(isBlackTree(tree), tree.value, tree.left, insNth(tree.right, idx - rank, v))
  }

  /* Based on Stefan Kahrs' Haskell version of Okasaki's Red&Black Trees
   * http://www.cse.unsw.edu.au/~dons/data/RedBlackTree.html */
  private[this] def del[B](tree: Tree[B], idx: Int): Tree[B] = if (tree eq null) null else {
    def balance(xv: B, tl: Tree[B], tr: Tree[B]) = if (isRedTree(tl)) {
      if (isRedTree(tr)) {
        RedTree(xv, tl.black, tr.black)
      } else if (isRedTree(tl.left)) {
        RedTree(tl.value, tl.left.black, BlackTree(xv, tl.right, tr))
      } else if (isRedTree(tl.right)) {
        RedTree(tl.right.value, BlackTree(tl.value, tl.left, tl.right.left), BlackTree(xv, tl.right.right, tr))
      } else {
        BlackTree(xv, tl, tr)
      }
    } else if (isRedTree(tr)) {
      if (isRedTree(tr.right)) {
        RedTree(tr.value, BlackTree(xv, tl, tr.left), tr.right.black)
      } else if (isRedTree(tr.left)) {
        RedTree(tr.left.value, BlackTree(xv, tl, tr.left.left), BlackTree(tr.value, tr.left.right, tr.right))
      } else {
        BlackTree(xv, tl, tr)
      }
    } else {
      BlackTree(xv, tl, tr)
    }
    def subl(t: Tree[B]) =
      if (t.isInstanceOf[BlackTree[_]]) t.red
      else sys.error("Defect: invariance violation; expected black, got " + t)

    def balLeft(xv: B, tl: Tree[B], tr: Tree[B]) = if (isRedTree(tl)) {
      RedTree(xv, tl.black, tr)
    } else if (isBlackTree(tr)) {
      balance(xv, tl, tr.red)
    } else if (isRedTree(tr) && isBlackTree(tr.left)) {
      RedTree(tr.left.value, BlackTree(xv, tl, tr.left.left), balance(tr.value, tr.left.right, subl(tr.right)))
    } else {
      sys.error("Defect: invariance violation")
    }
    def balRight(xv: B, tl: Tree[B], tr: Tree[B]) = if (isRedTree(tr)) {
      RedTree(xv, tl, tr.black)
    } else if (isBlackTree(tl)) {
      balance(xv, tl.red, tr)
    } else if (isRedTree(tl) && isBlackTree(tl.right)) {
      RedTree(tl.right.value, balance(tl.value, subl(tl.left), tl.right.left), BlackTree(xv, tl.right.right, tr))
    } else {
      sys.error("Defect: invariance violation")
    }
    def delLeft = if (isBlackTree(tree.left)) balLeft(tree.value, del(tree.left, idx), tree.right) else RedTree(tree.value, del(tree.left, idx), tree.right)
    def delRight(n: Int) = if (isBlackTree(tree.right)) balRight(tree.value, tree.left, del(tree.right, n)) else RedTree(tree.value, tree.left, del(tree.right, n))
    def append(tl: Tree[B], tr: Tree[B]): Tree[B] = if (tl eq null) {
      tr
    } else if (tr eq null) {
      tl
    } else if (isRedTree(tl) && isRedTree(tr)) {
      val bc = append(tl.right, tr.left)
      if (isRedTree(bc)) {
        RedTree(bc.value, RedTree(tl.value, tl.left, bc.left), RedTree(tr.value, bc.right, tr.right))
      } else {
        RedTree(tl.value, tl.left, RedTree(tr.value, bc, tr.right))
      }
    } else if (isBlackTree(tl) && isBlackTree(tr)) {
      val bc = append(tl.right, tr.left)
      if (isRedTree(bc)) {
        RedTree(bc.value, BlackTree(tl.value, tl.left, bc.left), BlackTree(tr.value, bc.right, tr.right))
      } else {
        balLeft(tl.value, tl.left, BlackTree(tr.value, bc, tr.right))
      }
    } else if (isRedTree(tr)) {
      RedTree(tr.value, append(tl, tr.left), tr.right)
    } else if (isRedTree(tl)) {
      RedTree(tl.value, tl.left, append(tl.right, tr))
    } else {
      sys.error("unmatched tree on append: " + tl + ", " + tr)
    }

    val rank = count(tree.left) + 1
    if (idx < rank) delLeft
    else if (idx > rank) delRight(idx - rank)
    else append(tree.left, tree.right)
  }

  private[this] def doConcat[B](left: Tree[B], right: Tree[B]): Tree[B] = {
    if (left eq null) right
    else if (right eq null) left
    else if (left.count == 1) insNth(right, 1, left.value)
    else if (right.count == 1) insNth(left, left.count + 1, right.value)
    else if (left.count < right.count)
      rebalance(last(left), deleteNth(left, left.count - 1), right)
    else
      rebalance(first(right), left, deleteNth(right, 0))
  }

  private[this] def doDrop[B](tree: Tree[B], n: Int): Tree[B] = {
    if (n <= 0) return tree
    if (n >= this.count(tree)) return null
    val count = this.count(tree.left)
    if (n > count) return doDrop(tree.right, n - count - 1)
    val newLeft = doDrop(tree.left, n)
    if (newLeft eq tree.left) tree
    else if (newLeft eq null) updNth(tree.right, n - count - 1, tree.value, overwrite = false)
    else rebalance(tree, newLeft, tree.right)
  }
  private[this] def doTake[B](tree: Tree[B], n: Int): Tree[B] = {
    if (n <= 0) return null
    if (n >= this.count(tree)) return tree
    val count = this.count(tree.left)
    if (n <= count) return doTake(tree.left, n)
    val newRight = doTake(tree.right, n - count - 1)
    if (newRight eq tree.right) tree
    else if (newRight eq null) updNth(tree.left, n, tree.value, overwrite = false)
    else rebalance(tree, tree.left, newRight)
  }
  private[this] def doSlice[B](tree: Tree[B], from: Int, until: Int): Tree[B] = {
    if (tree eq null) return null
    val count = this.count(tree.left)
    if (from > count) return doSlice(tree.right, from - count - 1, until - count - 1)
    if (until <= count) return doSlice(tree.left, from, until)
    val newLeft = doDrop(tree.left, from)
    val newRight = doTake(tree.right, until - count - 1)
    if ((newLeft eq tree.left) && (newRight eq tree.right)) tree
    else if (newLeft eq null) updNth(newRight, from - count - 1, tree.value, overwrite = false)
    else if (newRight eq null) updNth(newLeft, until, tree.value, overwrite = false)
    else rebalance(tree, newLeft, newRight)
  }

  private[this] final class Lst[A](val head: A, val tail: Lst[A]) {
    def foldLeft[B](z: B)(f: (B, A) => B): B = {
      var acc = z
      var these = this
      while (these ne null) {
        acc = f(acc, these.head)
        these = these.tail
      }
      acc
    }
  }
  private[this] final object Lst {
    //def isEmpty(l: List[_]): Boolean = l eq null
    def cons[B](x: B, xs: Lst[B]): Lst[B] = new Lst(x, xs)
  }

  // The zipper returned might have been traversed left-most (always the left child)
  // or right-most (always the right child). Left trees are traversed right-most,
  // and right trees are traversed leftmost.

  // Returns the zipper for the side with deepest black nodes depth, a flag
  // indicating whether the trees were unbalanced at all, and a flag indicating
  // whether the zipper was traversed left-most or right-most.

  // If the trees were balanced, returns an empty zipper
  private[this] def compareDepth[B](left: Tree[B], right: Tree[B]): (Lst[Tree[B]], Boolean, Boolean, Int) = {
    import Lst.cons
    // Once a side is found to be deeper, unzip it to the bottom
    def unzip(zipper: Lst[Tree[B]], leftMost: Boolean): Lst[Tree[B]] = {
      val next = if (leftMost) zipper.head.left else zipper.head.right
      if (next eq null) zipper
      else unzip(cons(next, zipper), leftMost)
    }
    // Unzip left tree on the rightmost side and right tree on the leftmost side until one is
    // found to be deeper, or the bottom is reached
    def unzipBoth(left: Tree[B],
      right: Tree[B],
      leftZipper: Lst[Tree[B]],
      rightZipper: Lst[Tree[B]],
      smallerDepth: Int): (Lst[Tree[B]], Boolean, Boolean, Int) = {
      if (isBlackTree(left) && isBlackTree(right)) {
        unzipBoth(left.right, right.left, cons(left, leftZipper), cons(right, rightZipper), smallerDepth + 1)
      } else if (isRedTree(left) && isRedTree(right)) {
        unzipBoth(left.right, right.left, cons(left, leftZipper), cons(right, rightZipper), smallerDepth)
      } else if (isRedTree(right)) {
        unzipBoth(left, right.left, leftZipper, cons(right, rightZipper), smallerDepth)
      } else if (isRedTree(left)) {
        unzipBoth(left.right, right, cons(left, leftZipper), rightZipper, smallerDepth)
      } else if ((left eq null) && (right eq null)) {
        (null, true, false, smallerDepth)
      } else if ((left eq null) && isBlackTree(right)) {
        val leftMost = true
        (unzip(cons(right, rightZipper), leftMost), false, leftMost, smallerDepth)
      } else if (isBlackTree(left) && (right eq null)) {
        val leftMost = false
        (unzip(cons(left, leftZipper), leftMost), false, leftMost, smallerDepth)
      } else {
        sys.error("unmatched trees in unzip: " + left + ", " + right)
      }
    }
    unzipBoth(left, right, null, null, 0)
  }

  private[this] def rebalance[B](tree: Tree[B], newLeft: Tree[B], newRight: Tree[B]) = {
    // This is like drop(n-1), but only counting black nodes
    def findDepth(zipper: Lst[Tree[B]], depth: Int): Lst[Tree[B]] = {
      if (isBlackTree(zipper.head))
        if (depth == 1) zipper else findDepth(zipper.tail, depth - 1)
      else findDepth(zipper.tail, depth)
    }

    // Blackening the smaller tree avoids balancing problems on union;
    // this can't be done later, though, or it would change the result of compareDepth
    val blkNewLeft = blacken(newLeft)
    val blkNewRight = blacken(newRight)
    val (zipper, levelled, leftMost, smallerDepth) = compareDepth(blkNewLeft, blkNewRight)

    if (levelled) {
      BlackTree(tree.value, blkNewLeft, blkNewRight)
    } else {
      val zipFrom = findDepth(zipper, smallerDepth)
      val union = if (leftMost) {
        RedTree(tree.value, blkNewLeft, zipFrom.head)
      } else {
        RedTree(tree.value, zipFrom.head, blkNewRight)
      }
      val zippedTree = zipFrom.tail.foldLeft(union: Tree[B]) { (tree, node) =>
        if (leftMost)
          balanceLeft(isBlackTree(node), node.value, tree, node.right)
        else
          balanceRight(isBlackTree(node), node.value, node.left, tree)
      }
      zippedTree
    }
  }

  /*
   * Forcing direct fields access using the @inline annotation helps speed up
   * various operations (especially smallest/greatest and update/delete).
   *
   * Unfortunately the direct field access is not guaranteed to work (but
   * works on the current implementation of the Scala compiler).
   *
   * An alternative is to implement the these classes using plain old Java code...
   */
  sealed abstract class Tree[+B](
    @(inline @getter) final val value: B,
    @(inline @getter) final val left: Tree[B],
    @(inline @getter) final val right: Tree[B])
    extends Serializable {
    @(inline @getter) final val count: Int = 1 + RedBlackRank.count(left) + RedBlackRank.count(right)
    def black: Tree[B]
    def red: Tree[B]
  }
  final class RedTree[+B](
    value: B,
    left: Tree[B],
    right: Tree[B]) extends Tree[B](value, left, right) {
    override def black: Tree[B] = BlackTree(value, left, right)
    override def red: Tree[B] = this
    override def toString: String = "RedTree(" + value + ", " + left + ", " + right + ")"
  }
  final class BlackTree[+B](
    value: B,
    left: Tree[B],
    right: Tree[B]) extends Tree[B](value, left, right) {
    override def black: Tree[B] = this
    override def red: Tree[B] = RedTree(value, left, right)
    override def toString: String = "BlackTree(" + value + ", " + left + ", " + right + ")"
  }

  object RedTree {
    @inline def apply[B](value: B, left: Tree[B], right: Tree[B]) = new RedTree(value, left, right)
    def unapply[B](t: RedTree[B]) = Some((t.value, t.left, t.right))
  }
  object BlackTree {
    @inline def apply[B](value: B, left: Tree[B], right: Tree[B]) = new BlackTree(value, left, right)
    def unapply[B](t: BlackTree[B]) = Some((t.value, t.left, t.right))
  }

  private[this] abstract class TreeIterator[B](root: Tree[B], start: Option[Int]) extends Iterator[B] {

    override def hasNext: Boolean = lookahead ne null

    protected def leftOf(tree: Tree[B]): Tree[B]
    protected def rightOf(tree: Tree[B]): Tree[B]

    override def next: B = lookahead match {
      case null =>
        throw new NoSuchElementException("next on empty iterator")
      case tree =>
        lookahead = findLeftMostOrPopOnEmpty(goRight(tree))
        tree.value
    }

    @tailrec
    private[this] def findLeftMostOrPopOnEmpty(tree: Tree[B]): Tree[B] =
      if (tree eq null) popNext()
      else if (leftOf(tree) eq null) tree
      else findLeftMostOrPopOnEmpty(goLeft(tree))

    private[this] def pushNext(tree: Tree[B]) {
      try {
        stackOfNexts(index) = tree
        index += 1
      } catch {
        case _: ArrayIndexOutOfBoundsException =>
          // see RedBlackTree
          assert(index >= stackOfNexts.length)
          stackOfNexts :+= null
          pushNext(tree)
      }
    }
    private[this] def popNext(): Tree[B] = if (index == 0) null else {
      index -= 1
      stackOfNexts(index)
    }

    private[this] var stackOfNexts = if (root eq null) null else {
      val maximumHeight = 2 * (32 - Integer.numberOfLeadingZeros(root.count + 2 - 1)) - 2 - 1
      new Array[Tree[B]](maximumHeight)
    }

    private[this] var index = 0
    private[this] var lookahead: Tree[B] = start map startFrom getOrElse findLeftMostOrPopOnEmpty(root) //findNext(root)

    /**
     * Find the leftmost subtree whose key is equal to the given key, or if no such thing,
     * the leftmost subtree with the key that would be "next" after it according
     * to the ordering. Along the way build up the iterator's path stack so that "next"
     * functionality works.
     */
    private[this] def startFrom(key: Int): Tree[B] = if (root eq null) null else {
      @tailrec def find(tree: Tree[B]): Tree[B] =
        if (tree eq null) popNext()
        else find(
          if (RedBlackRank.count(tree) <= key) goLeft(tree)
          else goRight(tree))
      find(root)
    }

    private[this] def goLeft(tree: Tree[B]) = {
      pushNext(tree)
      leftOf(tree)
    }

    private[this] def goRight(tree: Tree[B]) = rightOf(tree)

  }

  private[this] final class TreeIteratorFwd[B](tree: Tree[B], focus: Option[Int]) extends TreeIterator(tree, focus) {
    @inline override def leftOf(tree: Tree[B]): Tree[B] = tree.left
    @inline override def rightOf(tree: Tree[B]): Tree[B] = tree.right
  }
  private[this] final class TreeIteratorBwd[B](tree: Tree[B], focus: Option[Int]) extends TreeIterator(tree, focus) {
    @inline override def leftOf(tree: Tree[B]): Tree[B] = tree.right
    @inline override def rightOf(tree: Tree[B]): Tree[B] = tree.left
  }

  private[this] val CACHE_SIZE = 1 << 8

  // cache is populated on-demand
  // thread-safe since cache population is idempotent
  // in worst case few cpu cycles are wasted and structural sharing might not be optimal
  private[this] val cache: Array[Tree[_]] = new Array[Tree[_]](CACHE_SIZE);
  // initialize cache with seed values
  {
    cache(0) = null
    cache(1) = BlackTree(null, null, null)
  }

  /** Return a properly balanced and colored tree. Tree values are null. */
  def allocate[B](n: Int): Tree[B] = {

    /**
     * Return the number of nodes in perfect tree.
     * @param depth Depth of the tree.
     */
    def perfectTreeSize(depth: Int) = (1 << (depth + 1)) - 1

    /**
     * Return size of left hand side.
     * @param n Size of the parent tree.
     */
    // formula found with trial-and-error...
    def lhsSize(n: Int) = perfectTreeSize(depthOf((n + 1) / 3) - 1)

    /**
     * Return depth of tree.
     * @param n Size of the tree.
     */
    def depthOf(n: Int): Int = { // same as 32 - java.lang.Integer.numberOfLeadingZeros(n)
      var i = 0
      var a = n
      while (a > 0) {
        a >>= 1
        i += 1
      }
      i
    }

    def makeTree(n: Int): Tree[_] = {
      val e = if (n < CACHE_SIZE) cache(n) else null
      if (e == null && n > 0) {
        val lSize = lhsSize(n)
        val rSize = n - lSize - 1 // -1 to count root element
        val lhs = makeTree(lSize)
        val rhs = makeTree(rSize)
        val t = BlackTree(
          null,
          if (lSize > rSize) RedTree(null, lhs.left, lhs.right) else lhs, // TODO red trees could also be cached to improve structural sharing
          rhs)
        if (n < CACHE_SIZE) cache(n) = t
        t
      } else e
    }

    makeTree(n).asInstanceOf[Tree[B]]
  }

  // adapted from Scala 2.10 RedBlackTree tests
  private[rbvector] def invariants[B](t: Tree[B]) {

    def height(tree: Tree[_]): Int = if (tree eq null) 0 else (1 + math.max(height(tree.left), height(tree.right)))

    def rootIsBlack[A](t: Tree[B]) = isBlack(t)

    def areAllLeavesBlack[A](t: Tree[B]): Boolean = t match {
      case null => isBlack(t)
      case ne => List(ne.left, ne.right) forall areAllLeavesBlack
    }

    def areRedNodeChildrenBlack[A](t: Tree[B]): Boolean = t match {
      case RedTree(_, left, right) => List(left, right) forall (t => isBlack(t) && areRedNodeChildrenBlack(t))
      case BlackTree(_, left, right) => List(left, right) forall areRedNodeChildrenBlack
      case null => true
    }

    def blackNodesToLeaves[A](t: Tree[B]): List[Int] = t match {
      case null => List(1)
      case BlackTree(_, left, right) => List(left, right) flatMap blackNodesToLeaves map (_ + 1)
      case RedTree(_, left, right) => List(left, right) flatMap blackNodesToLeaves
    }

    def areBlackNodesToLeavesEqual[A](t: Tree[B]): Boolean = t match {
      case null => true
      case ne =>
        (blackNodesToLeaves(ne).distinct.size == 1
          && areBlackNodesToLeavesEqual(ne.left)
          && areBlackNodesToLeavesEqual(ne.right))
    }

    def heightIsBounded(t: Tree[_]): Boolean = height(t) <= (2 * (32 - Integer.numberOfLeadingZeros(count(t) + 2)) - 2)

    if (!rootIsBlack(t)) throw new IllegalStateException("root is not black")
    if (!areAllLeavesBlack(t)) throw new IllegalStateException("all leaves are not black")
    if (!areRedNodeChildrenBlack(t)) throw new IllegalStateException("children of red nodes are not black")
    if (!areBlackNodesToLeavesEqual(t)) throw new IllegalStateException("black nodes are not balanced")
    if (!heightIsBounded(t)) throw new IllegalStateException("height is not bounded")
  }

}
