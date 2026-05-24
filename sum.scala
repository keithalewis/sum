def choose(n: Int, k: Int): BigInt = {
  require(n >= 0 && k >= 0 && k <= n, "need 0 <= k <= n")
  val kk = math.min(k, n - k) // use symmetry to reduce work
  (1 to kk).foldLeft(BigInt(1)) { (acc, i) =>
    acc * (n - kk + i) / i
  }
}

def partitionBy[A, K](s: Set[A])(f: A => K): Set[Set[A]] =
  s.groupBy(f).values.map(_.toSet).toSet

def classesByKey[A, K](s: Set[A])(f: A => K): Map[K, Set[A]] =
  s.groupBy(f).view.mapValues(_.toSet).toMap

@main def run(): Unit =
  println(choose(50, 30))
