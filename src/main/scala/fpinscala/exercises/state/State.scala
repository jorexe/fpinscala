package fpinscala.exercises.state

import fpinscala.exercises.state.Input.{Coin, Turn}
import fpinscala.exercises.state.State.{get, modify, sequence, set, traverse}

import scala.annotation.tailrec


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (n, next) = rng.nextInt
    if (n < 0) (-(n + 1), next)
    else (n.abs, next)

  def double(rng: RNG): (Double, RNG) =
    val (n, next) = nonNegativeInt(rng)
    if (n == 0) (n.toDouble, next)
    else (1 / n.toDouble, next)

  def doubleViaMap(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)( a =>
      if (a == 0) a.toDouble
      else 1 / a.toDouble
    )(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (n1, next1) = nonNegativeInt(rng)
    val (n2, next2) = double(next1)
    ((n1, n2), next2)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val (p, next) = intDouble(rng)
    (p.swap, next)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (n1, next1) = double(rng)
    val (n2, next2) = double(next1)
    val (n3, next3) = double(next2)
    ((n1, n2, n3), next3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    @tailrec
    def go(count: Int, acc: List[Int], rng: RNG): (List[Int], RNG) =
      if (count <= 0) (acc, rng)
      else
        val (n, next) = rng.nextInt
        go(count - 1, n :: acc, next)

    go(count, Nil, rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rnc =>
      val (a, an) = ra(rnc)
      val (b, bn) = rb(an)
      (f(a, b), bn)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)(_ -> _)

  def intDoubleViaBoth(rng: RNG): ((Int, Double), RNG) =
    both(nonNegativeInt, double)(rng)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => rng =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) (mod, rng)
      else nonNegativeLessThan(n)(rng)
    )

  def nonNegativeLessThanBetter(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    )

  /**
   *
   * @param rs list((rng) => (x1, rng), (rng) => (x2, rng))
   * @return (rng) => ((x1, x2), rng)
   */
  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rng =>
      rs.foldRight[(List[A], RNG)]((Nil, rng))((rand, ls) =>
        val (x, next) = rand(ls._2)
        (x :: ls._1, next)
      )

  def sequenceBetter[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(Nil: List[A]))((r, acc) => map2(r, acc)(_ :: _))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (x, r1) = r(rng)
      f(x)(r1)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))


opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def map2Comprehension[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for
        a <- underlying
        b <- sb
      yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s =>
        val (a, s1) = run(s)
        f(a)(s1)
      )

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    yield ()

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def sequence[S, A](actions: List[State[S, A]]): State[S, List[A]] =
    actions.foldRight(unit[S, List[A]](Nil))((action, acc) => action.map2(acc)(_ :: _))

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight(unit[S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int):

  def empty: Boolean = candies == 0
  def insertCoin(): Machine =
    if (empty || !locked) this
    else Machine(false, candies, coins + 1)

  def turnKnob(): Machine =
    if (empty || locked) this
    else Machine(true, candies - 1, coins)

object Candy:
  /**
   * My implementation
   */
  def action(i: Input): State[Machine, Unit] = modify(m =>
    i match
      case Coin => m.insertCoin()
      case Turn => m.turnKnob()
  )

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State((m: Machine) => 
    val (_, m2) = sequence(inputs.map(action)).run(m)
    ((m2.coins, m2.candies), m2)
  )

  /**
   * Answer implementation
   * 
   * I prefer more my approach that put domain logic inside the machine
   * And checks only one condition instead of pattern matching
   */
  def update(i: Input): Machine => Machine = m =>
    i match
      case Coin => m.insertCoin()
      case Turn => m.turnKnob()

  def simulateMachineSolution(inputs: List[Input]): State[Machine, (Int, Int)] =
    for
      _ <- State.traverse(inputs)(i => State.modify(update(i)))
      s <- State.get
    yield (s.coins, s.candies)
