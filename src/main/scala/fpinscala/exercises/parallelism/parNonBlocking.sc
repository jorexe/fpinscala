import fpinscala.exercises.parallelism.Nonblocking.Par

import java.util.concurrent.{Executors, ThreadPoolExecutor, TimeUnit}

// Code will work with cached thread pool due to actor implementation
 val es = Executors.newFixedThreadPool(4)
//val es = Executors.newCachedThreadPool()
val pool = es.asInstanceOf[ThreadPoolExecutor]
val nums = 1 to 10000
val started = System.nanoTime()

val future = Par.reduce(nums)(_ + _).run(es)

val time = TimeUnit.MILLISECONDS.convert(System.nanoTime() - started, TimeUnit.NANOSECONDS)

val threads = pool.getPoolSize