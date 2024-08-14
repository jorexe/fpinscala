import fpinscala.exercises.parallelism.Actor

import java.util.concurrent.Executors

val es = Executors.newFixedThreadPool(4)

val echoActor = Actor[String](es):
  msg => println(s"Got message: '$msg'")

echoActor ! "Hello!"

echoActor ! "Goodbye!"
