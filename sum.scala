opaque type Time = Int

object Time:
  def apply(t: Int): Time =
    require(t >= 0, "need non-negative time")
    t

  extension (t: Time)
    def value: Int = t

def choose(n: Int, k: Int): Int = {
  require(n >= 0 && k >= 0 && k <= n, "need 0 <= k <= n")
  val j = math.min(k, n - k) // use symmetry to reduce work
  (1 to j).foldLeft(1) { (acc, i) =>
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

/**
  * Atom representing V_t = v.
  */
case class Atom(t: Time, v: Int)

type Function = Atom => Double

type Measure = Atom => Double

// One-sided random walk
def D(t: Time): Function = {
  (A: Atom) => choose(A.t.value, A.v) * math.pow(0.5, A.t.value)
}

type Filtration = Time => Seq[Atom]

def Entails(E: Atom, u: Time): Seq[Atom] = {
  // require E is an atom of A_t, and t < u
  // returns the atoms of A_u that entail E at time u
  (E.v to E.v + u - E.t).map(k => Atom(t = u, v = k)).toSeq
}

def value(E: Atom, u: Time, A: Function, D: Measure): Double = {
  // require E is an atom of A_t, and t < u
  Entails(E, u).iterator.map(e => A(e) * D(e)).sum // sum over atoms of A_u that entail E at time u
}

@main def run(): Unit =
  println(choose(50, 30))
  println(dot(Seq(1, 2, 3), Seq(4.0, 5.0, 6.0)))

  var v = Entails(Atom(t = Time(2), v = 1), Time(4))
  v.map(x => println(s"Entails: t=${x.t.value}, v=${x.v}"))

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