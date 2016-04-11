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

  private lazy val _weightMatrix = DenseMatrix.tabulate[Double](termStore.terms.length, termStore.terms.length)(_calculateWeights)

  def weights() = _weightMatrix

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
  private def _calculateWeights(j : Int, k : Int) = {

//    val quarter = n / 4
//
//    if (k == quarter) println("1/4 way there")
//    if (k == quarter + quarter) println("1/2 way there")
//    if (k == quarter + quarter + quarter) println("3/4 way there")

    val start = System.currentTimeMillis
    val weight = _weight(j, k)
    val end = System.currentTimeMillis

    if (weight > 1) println("WAT")
    //  print("time", end - start)
    weight
  }
}