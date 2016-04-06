package com.frankandrobot.chen.cluster

import com.frankandrobot.chen.DocTypes.Doc
import com.frankandrobot.chen.docs.{RawTermsByDocStore, TermStore}

import scala.math.log10


class ClusterWeights(termStore: TermStore,
                     clusterAnalysis: ClusterAnalysis,
                     rawTermsByDocStore: RawTermsByDocStore) {

  def weight(docWithRawTerms : Doc, termIndex : Int) = {

    val term = termStore.terms()(termIndex)

    clusterAnalysis.termFrequency(docWithRawTerms, term) * log10(clusterAnalysis.docFrequency(term))
  }

  def weight(docWithRawTerms: Doc, termIndex1 : Int, termIndex2 : Int) = {

    val term1 = termStore.terms()(termIndex1)
    val term2 = termStore.terms()(termIndex2)

    clusterAnalysis.termFrequency(docWithRawTerms, term1, term2) * log10(clusterAnalysis.docFrequency(term1, term2))
  }

}
