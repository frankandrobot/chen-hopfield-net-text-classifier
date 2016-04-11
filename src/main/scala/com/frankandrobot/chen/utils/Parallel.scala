package com.frankandrobot.chen.utils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object Parallel {

  /**
    * This is #map if it ran in parallel.
    *
    * Divide the sequence into blocks. Assign a CPU core to each block.
    * Do the map in each block.
    * Return the result
    *
    * @return
    */
  def map[T, R](list: IndexedSeq[T],
                action: T => R,
                maxCores: Int = Runtime.getRuntime.availableProcessors) = {

    val cores = Math.min(Runtime.getRuntime().availableProcessors(), maxCores)
    val n = list.length
    val max = if (n % cores == 0) { cores - 1 } else { cores }
    val stripWidth = n / cores

    // when n = 10, cores = 3 => max = 2, width = 3, strips = (0, 3) (3, 6) (6, 9) (9, 10)
    // when n = 10, cores = 2 => max = 1, width = 5, strips = (0, 5), (5, 10)

    val strips = (0 to max).map(i => (i * stripWidth, Math.min((i + 1) * stripWidth, n)))

    val calculation = strips.map(strip => {

      val range = strip._1 to strip._2 - 1

      // println("Starting", strip)

      Future(range.map(i => {
        action(list(i))
      }))
    })

    Future.sequence(calculation).map(_ flatten)
  }

  def sum[T](list : Seq[T],
             fn : T => Double,
             maxCores: Int = Runtime.getRuntime.availableProcessors) = {

    val cores = Math.min(Runtime.getRuntime().availableProcessors(), maxCores)
    val n = list.length
    val max = if (n % cores == 0) { cores - 1 } else { cores }
    val stripWidth = n / cores

    // when n = 10, cores = 3 => max = 2, width = 3, strips = (0, 3) (3, 6) (6, 9) (9, 10)
    // when n = 10, cores = 2 => max = 1, width = 5, strips = (0, 5), (5, 10)

    val strips = (0 to max).map(i => (i * stripWidth, Math.min((i + 1) * stripWidth, n)))

    val calculation = strips.map(strip => {

      val range = strip._1 to strip._2 - 1

      // println("Starting", strip)

      Future(FMath.sum(range, (i : Int) => fn(list(i))))
    })

    Future.sequence(calculation).map(_ sum)
  }
}
