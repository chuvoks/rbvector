
Alternative Vector implementation for Scala
===========================================

Provides fast:
- concatenation (++)
- patching
  - random element insertion/removal


Using with sbt
---------------

```
resolvers += "abzu" at "http://dl.bintray.com/content/abzu/maven"
libraryDependencies += "fi.rbvector" %% "rbvector" % "1.0.0"
```

Requires Scala 2.10 or 2.11.


Usage
-----

Nothing special, a drop in replacement for standard library [Vector](http://www.scala-lang.org/api/2.11.x/index.html#scala.collection.immutable.Vector).

Two convenience methods for random element removal and insertion are provided: inserted and removed.
(Method names where inspired by existing 
[updated](http://www.scala-lang.org/api/2.11.x/index.html#scala.collection.immutable.Vector@updated%28index:Int,elem:A%29:scala.collection.immutable.Vector[A])
method).

```scala
scala> import rbvector.Vector
import rbvector.Vector

// alias for patch(2, Vector(3), 0)
scala> Vector(1, 2, 4).inserted(2, 3) 
res0: rbvector.Vector[Int] = Vector(1, 2, 3, 4)

// alias for patch(1, Vector(), 1)
scala> res0.removed(1)
res1: rbvector.Vector[Int] = Vector(1, 3, 4)
```


Performance
-----------

In a nut shell, you want to use this if you need to
- concatenate vectors [1]
- use patch method
  - do random element insertion/removal
- have a truly general purpose IndexedSeq

The last point is bit subjective. Where standard Vector performs in
O(log_32 n) this has O(log_2 n). One way to illustrate this:
- log_2 (2^31 - 1) ~= 31.0
- log_32 (2^31 - 1) ~= 6.2

That is, you can expect standard Vector to be slightly faster in 
many operations. Standard Vector also benfits being based on arrays
whereas rbvector is based on red-black trees. 
But then again, standard Vector ++ and patch methods 
have complexity of O(n + m) where as rbvector has O(log(n + m)).

Some examples assuming one million element Vector(s):
- rbvector ++ is 25000 times faster than standard Vector
- standard Vector head() is 6.6 times faster and take() is 7.7 times faster

See the benchmark [results](benchmark/2014-12-12.results.txt) for details.

Notice that Scala 2.11 Vector ++ has been 
[optimized](https://github.com/scala/scala/commit/4234b34dd4e6563ba8a9b5080cc6a0da021848d3) 
to perform slightly better, especially when one of the collections is small.


Credits
-------

Most of the code is directly taken from Scala standard library classes
(scala.collection.immutable.RedBlackTree,
scala.collection.immutable.TreeMap and 
scala.collection.immutable.Vector).

RRBVector provided also some inspiration:
https://github.com/TiarkRompf/rrbtrees


License
-------

Apache License, Version 2.0
https://www.apache.org/licenses/LICENSE-2.0

