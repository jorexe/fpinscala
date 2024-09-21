package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.testing.Gen.SGen
import fpinscala.exercises.testing.Gen.SGen.*
import fpinscala.exercises.testing.Prop.*
import fpinscala.exercises.testing.Prop.Result.*

import scala.annotation.targetName

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

opaque type Prop = (MaxSize, TestCases, RNG) => Result

object Prop:

  opaque type SuccessCount = Int

  object SuccessCount:
    extension (x: SuccessCount) def toInt: Int = x

    def fromInt(x: Int): SuccessCount = x

  opaque type TestCases = Int

  object TestCases:
    extension (x: TestCases) def toInt: Int = x

    def fromInt(x: Int): TestCases = x

  opaque type MaxSize = Int

  object MaxSize:
    extension (x: MaxSize) def toInt: Int = x

    def fromInt(x: Int): MaxSize = x

  opaque type FailedCase = String

  object FailedCase:
    extension (f: FailedCase) def string: String = f

    def fromString(s: String): FailedCase = s

  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)
    case Proved

    def isFalsified: Boolean = this match
      case Passed => false
      case Proved => false
      case Falsified(_, _) => true

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop:
    (n, rng) =>
      randomLazyList(as)(rng)
        .zip(LazyList.from(0))
        .take(n)
        .map:
          case (a, i) =>
            try
              if (f(a)) then Result.Passed
              else Result.Falsified(a.toString, i)
            catch
              case e: Exception =>
                Result.Falsified(buildMsg(a, e), i)
        .find(_.isFalsified)
        .getOrElse(Result.Passed)
      
  @targetName("forAllSized")
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    (max, n, rng) =>
      val casesPerSize = (n.toInt - 1) / max.toInt + 1
      val props: LazyList[Prop] = 
        LazyList.from(0).take((n.toInt min max.toInt) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = 
        props.map[Prop](p => (max, n, rng) => p(max, casesPerSize, rng)).toList.reduce(_ && _)
      prop(max, n, rng)

  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"""
        test case: $s
        generated an exception: ${e.getMessage}
        stack trace:
        ${e.getStackTrace.mkString("\n")}
    """.trim

  def apply(f: (TestCases, RNG) => Result): Prop =
    (_, n, rng) => f(n, rng)

  extension (self: Prop)
    def check(
               maxSize: MaxSize = 100,
               testCases: TestCases = 100,
               rng: RNG = RNG.Simple(System.currentTimeMillis)
             ): Result =
      self(maxSize, testCases, rng)

    def run(
               maxSize: MaxSize = 100,
               testCases: TestCases = 100,
               rng: RNG = RNG.Simple(System.currentTimeMillis)
             ): Unit =
      self(maxSize, testCases, rng) match
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests")
        case Proved =>
          println(s"+ OK, proved property")

    @targetName("and")
    def &&(that: Prop): Prop =
      (max, n, rng) => self(max, n, rng) match
        case Passed | Proved => that(max, n, rng)
        case x => x

    @targetName("or")
    def ||(that: Prop): Prop =
      (max, n, rng) => self(max, n, rng) match
        case Falsified(_, _) => that(max, n, rng)
        case x => x

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

    def unsized: SGen[A] = _ => self

    def list: SGen[List[A]] = listOfN(_)
    
    def nonEmptyList: SGen[List[A]] = n => listOfN(1 max n)

  opaque type SGen[+A] = Int => Gen[A]

  object SGen:

    def apply[A](f: Int => Gen[A]): SGen[A] = f

    extension [A](self: SGen[A])

      def apply(n: Int): Gen[A] = self(n)

      def map[B](f: A => B): SGen[B] =
        n => self(n).map(f)

      def flatMap[B](f: A => SGen[B]): SGen[B] =
        n => self(n).flatMap(f(_)(n))
