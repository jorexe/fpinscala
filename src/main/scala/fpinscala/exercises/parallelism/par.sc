import fpinscala.exercises.parallelism.Par

import java.util.concurrent.{Executors, ThreadPoolExecutor, TimeUnit}

val es = Executors.newCachedThreadPool()
val pool = es.asInstanceOf[ThreadPoolExecutor]
val nums = 1 to 10000
val started = System.nanoTime()
val future = Par.reduce(nums)(_ + _).run(es)

val result = future.get(5, TimeUnit.SECONDS)

val time = TimeUnit.MILLISECONDS.convert(System.nanoTime() - started, TimeUnit.NANOSECONDS)

val threads = pool.getPoolSize