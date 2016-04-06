package com.frankandrobot.chen.classification

import breeze.linalg.DenseMatrix
import com.frankandrobot.chen.DocTypes.Doc
import com.frankandrobot.chen.cluster.ClusterWeights
import com.frankandrobot.chen.docs.{RawTermsByDocStore, TermStore}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.control.Breaks._


class ConnectionWeights(termStore: TermStore,
                        rawTermsByDocStore: RawTermsByDocStore,
                        clusterWeights: ClusterWeights,
                        threshold : Double = 0.1) {

  def n() = termStore.terms.length

  private lazy val _weightMatrix = DenseMatrix.zeros[Double](termStore.terms.length, termStore.terms.length)
  private var _init = false

  def weights() = {

    if (!_init) {

      // force load of weight matrix
      _weightMatrix(0, 0) = 0.0

      // waiting OK since nothing can happen until this calculation completes
      Await.result(_calculateWeights(), Duration.Inf)
      _init = true
    }

    _weightMatrix
  }

  /**
    * This is the main function
    *
    * @param j
    * @param k
    * @return
    */
  private def _weight(j : Int, k : Int) : Double = {

    val num = _shortcircuitSum(rawTermsByDocStore.docs, (doc : Doc) => clusterWeights.weight(doc, j, k))
    val denom = _shortcircuitSum(rawTermsByDocStore.docs, (doc : Doc) => clusterWeights.weight(doc, j))

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
  private def _shortcircuitSum(docs : Seq[Doc], fn : Doc => Double) : Double = {

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

    println("Starting ", x, y)

    val X = x._1 to x._2 - 1
    val Y = y._1 to y._2 - 1

    val quarter = (x._2 - x._1) / 4

    X foreach { j => {

      if (j == quarter) println("Quarter way there on", x, y);
      if (j == quarter * quarter) println("Halfway way there on", x, y);

      println(j, x, y)

      Y foreach { k => {

        _weightMatrix(j, k) = _weight(j, k)

      }}
    }}

    println("Finished ", x, y)
  }

  /**
    * Divide the matrix into blocks. Assign a CPU core to each block.
    *
    * @return
    */
  private def _calculateWeights() = {

    val cores = Math.min(Runtime.getRuntime().availableProcessors(), 4)
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