package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*

import java.util.concurrent.{ExecutorService, Executors}
import scala.annotation.targetName

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/
opaque type FailedCase = String
opaque type SuccessCount = Int

object SuccessCount:
  extension (sc: SuccessCount)
    @targetName("sum")
    def +(other: SuccessCount): SuccessCount = sc + other

trait Prop:
  self =>

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  @targetName("and")
  infix def &&(that: Prop): Prop = new Prop:
    override def check: Either[(FailedCase, SuccessCount), SuccessCount] =
      for
        c1 <- self.check
        c2 <- that.check
      yield c1 + c2


object Prop:


  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

opaque type Gen[+A] = State[RNG, A]

object Gen:
  def unit[A](a: => A): Gen[A] = State.unit(a)

  def choose(start: Int, stopInclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map( n => (n % (stopInclusive - start)) + start)

  def boolean: Gen[Boolean] =
    State(RNG.nonNegativeInt).map(_ % 2 == 0)

  def double: Gen[Double] =
    State(RNG.double)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if _ then g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    def runFirst(d: Double): Boolean =
      d < g1._2.abs / (g1._2.abs + g2._2.abs)

    double.flatMap(d => if runFirst(d) then g1._1 else g2._1)

  extension [A](self: Gen[A])

    def next(rng: RNG): (A, RNG) = self.run(rng)

    def listOfN(n: Int): Gen[List[A]] =
//      State.traverse((1 to n).toList)(n => self)
      State.sequence(List.fill(n)(self))

    def listOfN(size: Gen[Int]): Gen[List[A]] =
//      for
//        a <- self
//        b <- size
//      yield List.fill(b)(a)
      size.flatMap(self.listOfN)
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State.flatMap(self)(f)

//trait Gen[A]:
//  def map[B](f: A => B): Gen[B] = ???
//  def flatMap[B](f: A => Gen[B]): Gen[B] = ???

trait SGen[+A]
