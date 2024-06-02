import fpinscala.exercises.laziness.LazyList
import fpinscala.exercises.laziness.LazyList.{cons, fibs}

val ones: LazyList[Int] = cons(1, ones)

ones.take(5).toList

ones.exists(_ % 2 != 0)

ones.map(_ * 2).filter(_ % 2 == 0).take(5).toList

fibs.take(5).toList