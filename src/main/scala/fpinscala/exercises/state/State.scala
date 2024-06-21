package fpinscala.exercises.state

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

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = ???

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = ???

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      ???

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      ???

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      ???

  def apply[S, A](f: S => (A, S)): State[S, A] = f

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
