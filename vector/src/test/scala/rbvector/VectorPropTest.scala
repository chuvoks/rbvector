/*
 * Copyright (c) 2012-2014 Juha Heljoranta
 */
package rbvector
import org.scalatest.prop.PropertyChecks
import org.scalatest.PropSpec

import scala.util.Random

import java.util.NoSuchElementException
import scala.collection.immutable.{ Vector => SVect }

import org.scalatest.Matchers
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

class VectorPropTest extends PropSpec with PropertyChecks with Matchers {

  def invariants[A](t: Vector[A]) = { RedBlackRank.invariants(t.tree); t }

  implicit def arbVec[T](implicit a: Arbitrary[T]): Arbitrary[Vector[T]] =
    Arbitrary {
      val g = for {
        l <- Arbitrary.arbitrary[List[T]]
      } yield invariants(Vector.empty ++ l)
      Gen.sized(sz => g)
    }

  property("foreach/iterator consistency") {
    forAll { (v: Vector[Int]) =>
      val it = v.iterator
      v.foreach { element =>
        it.hasNext shouldBe true
        it.next shouldBe element
      }
      it.hasNext shouldBe false
    }
  }

  property("map/iterator consistency") {
    forAll { (v: Vector[Int]) =>
      val it = v.iterator
      v.map { element =>
        it.hasNext shouldBe true
        it.next shouldBe element
      }
      it.hasNext shouldBe false
    }
  }

  property("reverseMap/reverseIterator consistency") {
    forAll { (v: Vector[Int]) =>
      val it = v.reverseIterator
      v.reverseMap { element =>
        it.hasNext shouldBe true
        it.next shouldBe element
      }
      it.hasNext shouldBe false
    }
  }

  property("++ list") {
    forAll { (v: Vector[Int], l: List[Int]) =>
      invariants(v ++ l).toList shouldEqual v.toList ++ l
    }
  }

  property("++ set") {
    forAll { (v: Vector[Int], s: Set[Int]) =>
      invariants(v ++ s).toList shouldEqual v.toList ++ s
    }
  }

  property("++") {
    forAll { (v: Vector[Int], y: Vector[Int]) =>
      invariants(v ++ y).toList shouldEqual v.toList ++ y.toList
    }
  }

  property("++ s.c.i.vector") {
    forAll { (v: Vector[Int], y: scala.collection.immutable.Vector[Int]) =>
      invariants(v ++ y).toList shouldEqual v.toList ++ y.toList
    }
  }
  property("apply") {
    forAll { (v: Vector[Int]) =>
      val a = v.toArray
      for (i <- 0 until v.length)
        v(i) shouldBe a(i)
    }
  }

  property("length") {
    forAll { (v: Vector[Int]) =>
      (7 +: v).length shouldBe v.length + 1
    }
  }

  property("lengthCompare") {
    forAll { (v: Vector[Int], n: Int) =>
      v.lengthCompare(n) shouldBe v.toList.lengthCompare(n)
    }
  }

  property("head") {
    forAll { (v: Vector[Int]) =>
      whenever(v.nonEmpty) {
        v.head shouldBe v(0)
      }
    }
  }

  property("last") {
    forAll { (v: Vector[Int]) =>
      whenever(v.nonEmpty) {
        v.last shouldBe v(v.size - 1)
      }
    }
  }

  property("tail/head") {
    forAll { (v: Vector[Int]) =>
      whenever(v.nonEmpty) {
        invariants(v.head +: v.tail) shouldBe v
      }
    }
  }

  property("init/last") {
    forAll { (v: Vector[Int]) =>
      whenever(v.nonEmpty) {
        invariants(v.init :+ v.last) shouldBe v
      }
    }
  }
  property(":+") {
    forAll { (l: List[Int]) =>
      val v = l.foldLeft(Vector.empty[Int])((xs, x) => invariants(xs :+ x))
      v shouldBe l
    }
  }

  property("+:") {
    forAll { (l: List[Int]) =>
      val v = l.foldLeft(Vector.empty[Int])((xs, x) => invariants(x +: xs))
      v shouldBe l.reverse
    }
  }

  property("drop") {
    forAll { (v: Vector[Int], n: Int) =>
      invariants(v.drop(n)) shouldBe v.toList.drop(n)
    }
  }

  property("take") {
    forAll { (v: Vector[Int], n: Int) =>
      invariants(v.take(n)) shouldBe v.toList.take(n)
    }
  }

  property("map") {
    forAll { (v: Vector[Int]) =>
      invariants(v.map(_ + 1)) shouldBe v.toList.map(_ + 1)
    }
  }

  property("reverseMap") {
    forAll { (v: Vector[Int]) =>
      invariants(v.reverseMap(_ + 1)) shouldBe v.toList.reverseMap(_ + 1)
    }
  }

  property("reverse") {
    forAll { (v: Vector[Int]) =>
      invariants(v.reverse) shouldBe v.toList.reverse
    }
  }

  property("startsWith/take") {
    forAll { (v: Vector[Int], n: Int) =>
      v.startsWith(v.take(n)) shouldBe true
    }
  }

  property("endsWith/takeRight") {
    forAll { (v: Vector[Int], n: Int) =>
      v.endsWith(v.takeRight(n)) shouldBe true
    }
  }

  property("slice/take/drop") {
    forAll { (v: Vector[Int], from: Int, until: Int) =>
      invariants(v.slice(from, until)) shouldBe v.take(until).drop(from)
    }
  }

  property("slice") {
    forAll { (v: Vector[Int], from: Int, until: Int) =>
      invariants(v.slice(from, until)) shouldBe v.toList.slice(from, until)
    }
  }

  property("splitAt") {
    forAll { (v: Vector[Int], i: Int) =>
      v.splitAt(i) shouldBe v.toList.splitAt(i)
    }
  }

  property("takeRight") {
    forAll { (v: Vector[Int], n: Int) =>
      val a = invariants(v.takeRight(n))
      val b = v.toList.takeRight(n)
      a shouldBe b
    }
  }

  property("dropRight") {
    forAll { (v: Vector[Int], n: Int) =>
      invariants(v.dropRight(n)) shouldBe v.toList.dropRight(n)
    }
  }

  property("takeWhile") {
    forAll { (v: Vector[Int], n: Int) =>
      invariants(v.takeWhile(_ > n)) shouldBe v.toList.takeWhile(_ > n)
    }
  }

  property("span") {
    forAll { (v: Vector[Int], n: Int) =>
      v.span(_ > n) shouldBe v.toList.span(_ > n)
    }
  }

  property("patch") {
    forAll { (v: Vector[Int], p: Vector[Int], from: Int, replaced: Int) =>
      val a = invariants(v.patch(from, p, replaced))
      val b = v.toList.patch(from, p, replaced)
      a shouldBe b
    }
  }

  property("inserted") {
    forAll { (v: Vector[Int], i: Int) =>
      val a = invariants(v.inserted(i, 7))
      val b = v.toList.patch(i, Vector(7), 0)
      a shouldBe b
    }
  }

  property("removed") {
    forAll { (v: Vector[Int], i: Int) =>
      val a = invariants(v.removed(i))
      val b = v.toList.patch(i, List(), 1)
      a shouldBe b
    }
  }

  val tinyInteger = Gen.choose(0, 4)
  property("updated") {
    forAll(arbVec[Int].arbitrary, tinyInteger) { (v, i) =>
      whenever(v.nonEmpty && i < v.length) {
        val a = invariants(v.updated(i, 7))
        val b = v.toVector.updated(i, 7)
        a shouldBe b
      }
    }
  }

  property("random insertion") {
    forAll { (v: Vector[Int], seed: Long) =>
      val r = new util.Random(seed)
      val z = (Vector[Int](), List[Int]())
      (v.foldLeft(z) {
        case ((xs, ys), x) => {
          val i = r.nextInt(xs.length + 1)
          val (a, b) = (invariants(xs.inserted(i, x)), ys.patch(i, List(x), 0))
          a shouldBe b
          (a, b)
        }
      })._1.size shouldBe v.size
    }
  }

  property("random removal") {
    forAll { (v: Vector[Int], seed: Long) =>
      val r = new util.Random(seed)
      val z = (v, v.toList)
      (v.foldLeft(z) {
        case ((xs, ys), x) => {
          val i = r.nextInt(xs.length)
          val (a, b) = (invariants(xs.removed(i)), ys.patch(i, List(), 1))
          a shouldBe b
          (a, b)
        }
      })._1.isEmpty shouldBe true
    }
  }

}
