import fpinscala.exercises.testing.{Gen, Prop}

val smallInt = Gen.choose(-10, 10)

val maxProp = Prop.forAll(smallInt.nonEmptyList): l =>
  val max = l.max
  l.forall(_ <= max)

maxProp.run()

val sortedProp = Prop.forAll(smallInt.list): l =>
  val ls = l.sorted
//  val min = l.min
//  ls.foldLeft((true, min))((c, n) => (c._1 && c._2 <= n, n))._1
  (ls.isEmpty || ls.zip(ls.tail).forall(_ <= _)) &&
    ls.size == l.size &&
    ls.forall(l.contains) && 
    l.forall(ls.contains)

sortedProp.run()