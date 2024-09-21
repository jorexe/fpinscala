package fpinscala.exercises.typeclasses

trait Showable[A]:
  extension (a: A) def show: String

given Showable[Int] with
  extension (a: Int)
    def show: String = s"I'm an int $a"

given Showable[String] with
  extension (a: String)
    def show: String = s"I'm a string $a"

case class Person(firstName: String, lastName: String)

given Showable[Person] with
  extension (p: Person) def show: String =
    s"I'm a person and my name is ${p.firstName} ${p.lastName}"

def showAll[A: Showable](as: List[A]): Unit =
  as.foreach(a => println(a.show))

case class Pet(name: String)

@main def run =
  println("asd".show)
  println(123.show)
  println(Person("John", "Doe").show)
//  println(Pet("Butter").show) Won't compile
  showAll(List(Person("John", "Doe"), Person("Jane", "Doe")))
//  showAll(List(Pet("Butter"), Pet("Max"))) Won't compile