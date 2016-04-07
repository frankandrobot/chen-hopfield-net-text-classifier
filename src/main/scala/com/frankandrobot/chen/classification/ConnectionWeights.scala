package com.frankandrobot.chen.classification

import breeze.linalg.DenseMatrix
import com.frankandrobot.chen.DocTypes.Doc
import com.frankandrobot.chen.cluster.ClusterWeights
import com.frankandrobot.chen.docs.{DocStore, TermStore}
import com.frankandrobot.chen.utils.Concurrent
import com.frankandrobot.chen.utils.Memoize._

import scala.concurrent.Await
import scala.concurrent.duration.Duration


class ConnectionWeights(termStore: TermStore,
                        docStore: DocStore,
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
      _calculateWeights()
      //Await.result(_calculateWeights(), Duration.Inf)
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

   val num = Await.result(Concurrent.sum(docStore.docs, (doc : Doc) => clusterWeights.weight(doc, j, k)), Duration.Inf)
   val denom = _denomFn(j)

    val w = num / denom

    //println(j, k, w)

    if (w <= threshold || w.isNaN) { 0.0 }
    else { w }
  }

  private def _denum(j : Int) =
    Await.result(Concurrent.sum(docStore.docs, (doc : Doc) => clusterWeights.weight(doc, j)), Duration.Inf)

  private def _denomFn = memoize(_denum _)

  private def _blockWeight(x : Strip, y : Strip) = {

    //println("Starting ", x, y)

    val X = x._1 to x._2 - 1
    val Y = y._1 to y._2 - 1

    val quarter = (x._2 - x._1) / 4

    X foreach { j => {

      if (j == quarter) println("1/4 way there on", x, y);
      if (j == quarter + quarter) println("1/2 way there on", x, y);
      if (j == quarter + quarter + quarter) println("3/4 way there on", x, y);

     println(j, x, y)

      Y foreach { k => {

        _weightMatrix(j, k) = _weight(j, k)

      }}
    }}

    //println("Finished ", x, y)
  }

  /**
    * Divide the matrix into blocks. Assign a CPU core to each block.
    *
    * @return
    */
  private def _calculateWeights() = {

//    val cores = 1 //Math.max(Runtime.getRuntime().availableProcessors() - 2, 1)
//    val max = if (n % cores == 0) { cores - 1} else { cores }
//    val stripWidth = n / cores
//
//    // when n = 10, cores = 3 => max = 2, width = 3, patches = (0, 3) (3, 6) (6, 9) (9, 10)
//    // when n = 10, cores = 2 => max = 1, width = 5, patches = (0, 5), (5, 10)
//
//    val strips = (0 to max).map(i => (i * stripWidth, Math.min((i + 1) * stripWidth, n)))
//    val blocks = (for (x <- strips; y <- strips) yield (x,y))
//
//    println(blocks)
//    // blocks.foreach(println)
//
//    val calculateBlockWeights = blocks.map(block => _blockWeight(block._1, block._2))
//
//    Future.sequence(calculateBlockWeights)

    _blockWeight((0, n), (0, n))
  }

  type Strip = (Int, Int)
}