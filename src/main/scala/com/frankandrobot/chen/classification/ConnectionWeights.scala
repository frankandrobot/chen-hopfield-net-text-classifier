package com.frankandrobot.chen.classification

import breeze.linalg.DenseMatrix
import com.frankandrobot.chen.DocTypes.DocWithRawTerms
import com.frankandrobot.chen.cluster.ClusterWeights
import com.frankandrobot.chen.docs.{RawTermsByDocStore, TermStore}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.Breaks._


class ConnectionWeights(termStore: TermStore,
                        rawTermsByDocStore: RawTermsByDocStore,
                        clusterWeights: ClusterWeights,
                        threshold : Double = 0.1) {

  def n() = termStore.terms.length

  lazy val weights = DenseMatrix.zeros[Double](termStore.terms.length, termStore.terms.length)

  /**
    * This is the main function
    *
    * @param j
    * @param k
    * @return
    */
  private def _weight(j : Int, k : Int) : Double = {

    val num = _shortcircuitSum(rawTermsByDocStore.docs, (doc : DocWithRawTerms) => clusterWeights.weight(doc, j, k))
    val denom = _shortcircuitSum(rawTermsByDocStore.docs, (doc : DocWithRawTerms) => clusterWeights.weight(doc, j))

    val w = num / denom

    //println(j, k, w)

    if (w <= threshold || w.isNaN) { 0.0 }
    else { w }
  }

  /**
    * Iterate thru each doc and call the given function. Sum the results and return 0.0 if ever NaN is encountered
    *
    * @param docs
    * @param fn
    * @return
    */
  private def _shortcircuitSum(docs : Seq[DocWithRawTerms], fn : DocWithRawTerms => Double) : Double = {

    var sum = 0.0
    var quant = 0.0

    breakable { for (i <- docs) {

      quant = fn(i)

      if (quant.isNaN) break
      else sum += quant
    } }

    sum
  }

  private def _blockWeight(x : Strip, y : Strip) = Future {

    val X = x._1 to x._2 - 1
    val Y = y._1 to y._2 - 1

    X foreach { j => Y foreach { k => weights(j, k) = _weight(j, k)}}
  }

  /**
    * Divide the matrix into blocks. Assign a CPU core to each block.
    *
    * @return
    */
  def calculateWeights() = {

    // force load of weight matrix
    weights(0, 0) = 0.0

    val cores = Runtime.getRuntime().availableProcessors()
    val max = if (n % cores == 0) { cores - 1} else { cores }
    val stripWidth = n / cores

    // when n = 10, cores = 3 => max = 2, width = 3, patches = (0, 3) (3, 6) (6, 9) (9, 10)
    // when n = 10, cores = 2 => max = 1, width = 5, patches = (0, 5), (5, 10)

    val strips = (0 to max).map(i => (i * stripWidth, Math.min((i + 1) * stripWidth, n)))
    val blocks = for (x <- strips; y <- strips) yield (x,y)

    // blocks.foreach(println)

    val calculateBlockWeights = blocks.map(block => _blockWeight(block._1, block._2))

    Future.sequence(calculateBlockWeights)
  }

  type Strip = (Int, Int)

}