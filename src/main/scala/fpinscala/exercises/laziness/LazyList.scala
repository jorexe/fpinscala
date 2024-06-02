package fpinscala.exercises.laziness

import fpinscala.answers.laziness.LazyList
import fpinscala.exercises.laziness.LazyList.{cons, empty, unfold}

import scala.annotation.tailrec

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] =
    this match
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList

  def toListRec: List[A] =
    @tailrec
    def go(ll: LazyList[A], acc: List[A]): List[A] = ll match
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc.reverse
    go(this, Nil)


  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty


  def drop(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this

  def takeWhileImp(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty

  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean = !exists(!p(_))

  def forAll2(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): LazyList[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty[A])((a, b) => if f(a) then cons(a, b) else b)

  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)((a, b) => cons(a, b))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def betterFind(p: A => Boolean): Option[A] = filter(p).headOption

  def startsWith[B >: A](s: LazyList[B]): Boolean = this.zipAll(s).forAll((a, b) => (a, b) match
    case (Some(a), Some(b)) => a == b
    case (_, None) => true
    case _ => false
  )

  def betterStartsWith[B >: A](s: LazyList[B]): Boolean =
    this.zipAll(s).takeWhile(_._2.isDefined).forAll2(_ == _)

  def tails: LazyList[LazyList[A]] =
    unfold(this):
      case Cons(h, t) => Some((Cons(h, t), t()))
      case _ => None
    .append(LazyList(empty))

  def hasSubsequence[B >: A](s: LazyList[B]): Boolean = tails.exists(_.startsWith(s))

  // Not performant
  // Breaks laziness due to unfold going from left to right
  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] =
    unfold(this):
      case Cons(h, t) => Some((Cons(h, t).foldRight(init)(f), t()))
      case _ => None
    .append(LazyList(init))

  def betterScanRight[B](init: B)(f: (A, => B) => B): LazyList[B] =
      foldRight((init, LazyList(init))): (a, b0) =>
        lazy val b1 = b0
        val b2 = f(a, b1._1)
        (b2, cons(b2, b1._2))
      ._2

  def mapViaUnfold[B](f: A => B): LazyList[B] = unfold(this):
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None

  def takeViaUnfold(n: Int): LazyList[A] = unfold((this, n)):
    case (Cons(h, t), 1) => Some((h(), (empty, 0)))
    case (Cons(h, t), n) if n >= 1 => Some((h(), (t(), n-1)))
    case _ => None

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] = unfold(this):
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None

  def zipWith[B, C](list: LazyList[B], f: (A, B) => C): LazyList[C] = unfold((this, list)):
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] = unfold((this, that)):
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), empty)))
    case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
    case _ => None

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = cons(1, ones)

  def continuallyOld[A](a: A): LazyList[A] = cons(a, continually(a))

  def continually[A](a: A): LazyList[A] =
    lazy val single: LazyList[A] = cons(a, single)
    single

  def from(n: Int): LazyList[Int] = cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] =
    def start(prev: Int, curr: Int): LazyList[(Int, Int)] =
      cons((prev, curr), start(curr, prev + curr))
    start(0, 1).map(_._1)

  def oldUnfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    def go(curr: S): LazyList[(A, S)] = f(curr) match
      case Some(na, ns) => cons((na, curr), go(ns))
      case None => empty
    go(state).map(_._1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some(na, ns) => cons(na, unfold(ns)(f))
      case None => empty

  lazy val fibsViaUnfold: LazyList[Int] = unfold((0, 1))((a, b) => Some((a, (b, a + b))))

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(s => Some(s, s + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(a)(Some(a, _))

  // There is no need to store state in here, we can use unit
  def continuallyViaUnfold2[A](a: A): LazyList[A] = unfold(())(Some(a, _))

  lazy val onesViaUnfold: LazyList[Int] = continuallyViaUnfold(1)
