package com.frankandrobot.chen.cluster

import breeze.linalg.DenseMatrix
import com.frankandrobot.chen.DocTypes.DocWithRawTerms
import com.frankandrobot.chen.docs.{RawTermsByDocStore, TermStore}

import scala.math.log10

class ClusterWeights(termStore: TermStore,
                     clusterAnalysis: ClusterAnalysis,
                     rawTermsByDocStore: RawTermsByDocStore,
                     threshold : Double) {

  def weight(termIndex : Int, docWithRawTerms : DocWithRawTerms) = {

    val term = termStore.terms()(termIndex)

    clusterAnalysis.termFrequency(term, docWithRawTerms) * log10(clusterAnalysis.docFrequency(term))
  }

  def weight(termIndex1 : Int, termIndex2 : Int, docWithRawTerms: DocWithRawTerms) = {

    val term1 = termStore.terms()(termIndex1)
    val term2 = termStore.terms()(termIndex2)

    clusterAnalysis.termFrequency(term1, term2, docWithRawTerms) * log10(clusterAnalysis.docFrequency(term1, term2))
  }

  lazy val weights = DenseMatrix.tabulate(termStore.terms.length, termStore.terms.length){ case (i, j) =>

    val num = rawTermsByDocStore.docs.foldLeft(0.0)(_ + weight(i, j, _))
    val denom = rawTermsByDocStore.docs.foldLeft(0.0)(_ + weight(i, _))

    val w = num / denom

    if (w > threshold) { w }
    else { 0.0 }
  }
}
