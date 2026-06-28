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


/**
  * Atom representing V_t = v.
  */
case class Atom(t: Time, v: Int)

type Function = Atom => Double

type Measure = Atom => Double

type Filtration = Time => Iterable[Atom]

type StoppingTime = Seq[(Time, Iterable[Atom])]

// One-sided random walk
def D(t: Time): Function = {
  (A: Atom) => choose(A.t.value, A.v) * math.pow(0.5, A.t.value)
}

// Atoms F in A_u with F subset E.
def EntailingAtoms(E: Atom, u: Time): Iterable[Atom] = {
  require(u > E.t, "need u > E.t")
  (E.v to E.v + u - E.t).map(k => Atom(t = u, v = k)).toIterable
}

// Atoms in the event {tau = u}.
def StoppingAtoms(tau: StoppingTime, u: Time): Set[Atom] =
  tau.collectFirst { case (`u`, atoms) => atoms.toSet }.getOrElse(Set.empty)

// {F in A_u | F subset E cap {tau = u}}
def Atoms(E: Atom, u: Time, tau: StoppingTime): Iterable[Atom] = {
  val stopU = StoppingAtoms(tau, u)
  EntailingAtoms(E, u).filter(stopU.contains)
}

// (A_u D_u)|A_t
def Value(u: Time, A: Function, D: Measure): Measure = {
  // require E is an atom of A_t, and t < u
  (E: Atom) => EntailingAtoms(E, u).iterator.map(e => A(e) * D(e)).sum // sum over atoms of A_u that entail E at time u
}

// (1(tau > t) A_tau D_tau)|A_t
def StoppedValue(tau: StoppingTime, A: Function, D: Measure): Measure =
  (E: Atom) =>
    tau.iterator
      .filter { case (u, _) => u > E.t }
      .flatMap { case (u, _) => Atoms(E, u, tau).iterator.map(F => A(F) * D(F)) }
      .sum

@main def run(): Unit =
  println(choose(5, 3))

  val e = Atom(t = Time(2), v = 1)
  val atoms = EntailingAtoms(e, Time(4))
  atoms.foreach(x => println(s"Atoms: t=${x.t.value}, v=${x.v}"))
  var d = D(Time(4))(Atom(t = Time(4), v = 2))
  println(s"D(4)(Atom(4,2)) = $d")
  def A(a: Atom): Double = a.v.toDouble
  val value = Value(Time(4), A, D(Time(4)))
  println(value(e))

  val tau: StoppingTime = Seq(
    (Time(3), Iterable(Atom(Time(3), 2), Atom(Time(3), 3))),
    (Time(4), Iterable(Atom(Time(4), 3), Atom(Time(4), 4), Atom(Time(4), 5)))
  )
  def discount(a: Atom): Double = D(a.t)(a)
  val stopped = StoppedValue(tau, A, discount)
  println(stopped(e))

