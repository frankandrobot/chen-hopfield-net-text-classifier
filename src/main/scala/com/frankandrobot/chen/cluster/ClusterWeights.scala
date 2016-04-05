package com.frankandrobot.chen.cluster

import breeze.linalg.DenseMatrix
import com.frankandrobot.chen.DocTypes.DocWithRawTerms
import com.frankandrobot.chen.docs.{RawTermsByDocStore, TermStore}

import scala.math.log10

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

    val num = rawTermsByDocStore.docs.foldLeft(0.0)(_ + weight(_, j, k))
    val denom = rawTermsByDocStore.docs.foldLeft(0.0)(_ + weight(_, j))

    val w = num / denom

    // println(j, k, w)

    if (w > threshold) { w }
    else { 0.0 }
  }
}
