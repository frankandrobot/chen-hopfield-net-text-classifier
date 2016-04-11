package com.frankandrobot.chen.classification

import breeze.linalg.DenseMatrix
import com.frankandrobot.chen.DocTypes.Doc
import com.frankandrobot.chen.cluster.ClusterWeights
import com.frankandrobot.chen.docs.{DocStore, TermStore}
import com.frankandrobot.chen.utils.Parallel

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

   val num = Await.result(Parallel.sum(docStore.docs, (doc : Doc) => clusterWeights.weight(doc, j, k)), Duration.Inf)
   val denom = Await.result(Parallel.sum(docStore.docs, (doc : Doc) => clusterWeights.weight(doc, j)), Duration.Inf)

    val w = num / denom

    //println(j, k, w)

    if (w <= threshold || w.isNaN) { 0.0 }
    else { w }
  }

  /**
    * Divide the matrix into blocks. Assign a CPU core to each block.
    *
    * @return
    */
  private def _calculateWeights() = {

    val range = 0 to n - 1

    val quarter = n / 4

    range foreach { j => {

      if (j == quarter) println("1/4 way there")
      if (j == quarter + quarter) println("1/2 way there")
      if (j == quarter + quarter + quarter) println("3/4 way there")

      val start = System.currentTimeMillis

      range foreach { k => {

        _weightMatrix(j, k) = _weight(j, k)

      }}

      val end = System.currentTimeMillis

      println("time", end - start)
    }}
  }
}