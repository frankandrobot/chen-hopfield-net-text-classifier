package com.frankandrobot.chen.utils

import com.frankandrobot.chen.utils.Memoize._


object FMath {

  def sum[T](list : Seq[T], fn : T => Double) : Double = {

    var sum = 0.0
    var q = 0.0

    for (i <- list) {

      val value = fn(i)

      if (value.isInfinity || value.isNaN) { return value }

      sum += fn(i)
    }

    sum
  }

  def sum[T](list : Seq[T], fn : T => Int) : Int = {

    var sum = 0

    for (i <- list) {

      sum += fn(i)
    }

    sum
  }

  /**
    * For some reason, this is slower than Math.log10. Probably because it already uses a hash table
    * @deprecated use Math.log10
    *
    * @return
    */
  def log10 = memoize(Math.log10 _)

  def setup() = {

    for(i <- 1 to 1000) { log10(i) }
  }
}
