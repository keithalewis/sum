def choose(n: Int, k: Int): BigInt = {
  require(n >= 0 && k >= 0 && k <= n, "need 0 <= k <= n")
  val j = math.min(k, n - k) // use symmetry to reduce work
  (1 to j).foldLeft(BigInt(1)) { (acc, i) =>
    acc * (n - j + i) / i
  }
}

// 0, 1, 2, ...
//def Time() = Iterator.from(0)

def dot[A: Numeric, B: Numeric](xs: Seq[A], ys: Seq[B]): Double =
  require(xs.length == ys.length, "dot: sequences must have the same length")
  val na = summon[Numeric[A]]
  val nb = summon[Numeric[B]]
  xs.iterator
    .zip(ys.iterator)
    .map((x, y) => na.toDouble(x) * nb.toDouble(y))
    .sum

@main def run(): Unit =
  println(choose(50, 30))
  println(dot(Seq(1, 2, 3), Seq(4.0, 5.0, 6.0)))

/*


def partitionBy[A, K](s: Set[A])(f: A => K): Set[Set[A]] =
  s.groupBy(f).values.map(_.toSet).toSet

def classesByKey[A, K](s: Set[A])(f: A => K): Map[K, Set[A]] =
  s.groupBy(f).view.mapValues(_.toSet).toMap

extension [A, B](f: A => B)
  def zipWith(g: A => B)(op: (B, B) => B): A => B =
    a => op(f(a), g(a))

// val h2 = f.zipWith(g)(_ + _)

import scala.annotation.targetName

extension [A, B](f: A => B)
// JVM name for interop
@targetName("pointwiseCombine")
infix def <+>(g: A => B)(using op: (B, B) => B): A => B =
a => op(f(a), g(a))

//Common symbols you can use in operator names include:
//+ - * / % ^ & | ! = < > : ? ~ \

@main def run(): Unit =
  println(choose(50, 30))
*/