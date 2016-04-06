package com.frankandrobot.chen.cluster

import breeze.linalg.DenseMatrix
import com.frankandrobot.chen.DocTypes.DocWithRawTerms
import com.frankandrobot.chen.docs.{RawTermsByDocStore, TermStore}

import scala.math.log10
import scala.util.control.Breaks._


class ClusterWeights(termStore: TermStore,
                     clusterAnalysis: ClusterAnalysis,
                     rawTermsByDocStore: RawTermsByDocStore,
                     threshold : Double = 0.1) {

  def weight(docWithRawTerms : DocWithRawTerms, termIndex : Int) = {

    val term = termStore.terms()(termIndex)

    clusterAnalysis.termFrequency(docWithRawTerms, term) * log10(clusterAnalysis.docFrequency(term))
  }

  def weight(docWithRawTerms: DocWithRawTerms, termIndex1 : Int, termIndex2 : Int) = {

    val term1 = termStore.terms()(termIndex1)
    val term2 = termStore.terms()(termIndex2)

    clusterAnalysis.termFrequency(docWithRawTerms, term1, term2) * log10(clusterAnalysis.docFrequency(term1, term2))
  }

  lazy val weights = DenseMatrix.tabulate(termStore.terms.length, termStore.terms.length){ case (j, k) =>

    val num = ClusterWeights._shortcircuitSum(rawTermsByDocStore.docs, (doc : DocWithRawTerms) => weight(doc, j, k))
    val denom = ClusterWeights._shortcircuitSum(rawTermsByDocStore.docs, (doc : DocWithRawTerms) => weight(doc, j))

    val w = num / denom

    //println(j, k, w)

    if (w <= threshold || w.isNaN) { 0.0 }
    else { w }
  }
}

object ClusterWeights {

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
}