/*
 * Copyright (c) 2012-2014 Juha Heljoranta
 */
package rbvector

import org.openjdk.jmh.annotations._

import scala.{ Vector => SVec }

@State(Scope.Thread)
class VectorBench {

  @Param(Array("1", "10", "100", "1000", "10000", "100000", "1000000"))
  var size: Int = _

  @Param(Array("vector", "rbvector"))
  var typeof: String = _

  var v: IndexedSeq[Int] = _

  var vOne: IndexedSeq[Int] = _

  var vEmpty: IndexedSeq[Int] = _

  var randomIndexes: Array[Int] = _

  var x = 42

  var index: Int = _ // approx 62 % of the size

  @Setup
  def setup(): Unit = {
    val data = 0 until size
    typeof match {
      case "vector" => {
        v = data.toVector
        vOne = SVec(x)
        vEmpty = SVec.empty
      }
      case "rbvector" => {
        v = Vector.empty ++ data
        vOne = Vector(x)
        vEmpty = Vector.empty
      }
    }
    randomIndexes = new util.Random(x).shuffle(v).toArray
    if (size == 1) index = 0
    else {
      val goldenRatio = (1 + math.sqrt(5)) / 2
      index = math.round(size / goldenRatio).toInt
    }
  }

  @Benchmark
  def concatSelf =
    v ++ v

  @Benchmark
  def concatOne =
    v ++ vOne

  @Benchmark
  def concatArray =
    v ++ randomIndexes

  @Benchmark
  def append =
    v :+ x

  @Benchmark
  def head =
    v.head

  @Benchmark
  def tail =
    v.tail

  @Benchmark
  def take =
    v.take(index)

  @Benchmark
  def apply =
    v(index)

  @Benchmark
  def applyAllRandom =
    { var j = 0; randomIndexes.foreach(i => j = v(i)); j }

  @Benchmark
  def map =
    v.map(_ + 1)

  @Benchmark
  def foreach =
    { var i = 0; v.foreach(i = _); i }

  @Benchmark
  def foldLeft =
    v.foldLeft(0)(_ + _)

  @Benchmark
  def reverse =
    v.reverse

  @Benchmark
  def iteratorSize =
    v.iterator.size

  @Benchmark
  def splitAt =
    v.splitAt(index)

  @Benchmark
  def slice =
    v.slice(index / 2, index)

  @Benchmark
  def filterTrue =
    v.filter(_ => true)

  @Benchmark
  def updated =
    v.updated(index, x)

  @Benchmark
  def patch =
    v.patch(index, v, size / 4)

  @Benchmark
  def patchInsertOne =
    v.patch(index, vOne, 0)

  @Benchmark
  def patchRemoveOne =
    v.patch(index, vEmpty, 1)

}

